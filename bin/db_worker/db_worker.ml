open! Std
open Db.Worker_api
open Brrer
open! Brr
module IDB = Brr_io.Indexed_db
open Db.Stores

let () = Random.self_init ()

let map_error f =
  let open Fut.Syntax in
  let+ f = f in
  Result.map_err (fun jv -> `Jv jv) f

let as_fut q = IDB.Request.fut q |> map_error

let fut_of_array (fs : 'a Fut.t array) : 'a array Fut.t =
  let fut p = Jv.obj [| ("fut", p) |] in
  let promise' f = Jv.get (Jv.repr f) "fut" in
  let arr = Jv.of_array promise' fs in
  let all = Jv.Promise.all arr in
  let to_array l = Jv.Promise.resolve (Jv.to_array Obj.magic l) in
  Obj.magic @@ fut @@ Jv.Promise.bind all to_array

module Worker () = struct
  let last_view : (int * Db.Generic_schema.Track.Key.t array) ref =
    ref (-1, [||])

  let view_memo : (int, Tracks_store.Primary_key.t array) Hashtbl.t =
    Hashtbl.create 64

  let invalidate_cache () = Hashtbl.reset view_memo

  let check_db idb source =
    let server_id, source = source in
    let report status =
      last_view := (-1, [||]);
      dispatch_event Servers_status_update (server_id, status)
    in
    Db.Sync.check_and_sync ~report ~source idb
    |> Fut.map (fun res ->
           invalidate_cache ();
           res)

  let idb =
    let idb, set_idb = Fut.create () in
    let _ = Db.with_idb @@ fun idb -> ignore (set_idb @@ Ok idb) in
    idb

  let get_store (type t') (module Store : IDB.Store_intf with type t = t')
      ?(mode = IDB.Transaction.Readonly) () : (t', error) Fut.result =
    let open Fut.Result_syntax in
    let+ idb = idb in
    IDB.Database.transaction [ (module Store) ] ~mode idb
    |> IDB.Transaction.object_store (module Store)

  let match_filter ~filter elements =
    List.fold_left filter ~init:true ~f:(fun acc -> function
      | Db.View.Selection.All -> acc && true
      | One_of one_of -> acc && not (Int.Set.disjoint elements one_of)
      | None_of none_of ->
          acc && (Int.Set.is_empty none_of || Int.Set.disjoint elements none_of))

  let get_view_keys store
      ({ Db.View.kind = _; src_views; sort; filters } as req) =
    (* todo: staged memoization + specialized queries using indexes *)
    let open Fut.Result_syntax in
    try Fut.ok @@ Hashtbl.find view_memo @@ Db.View.hash req
    with Not_found ->
      let n = Performance.now_ms G.performance in
      let+ all_keys = Db.Stores.Tracks_store.get_all_keys store |> as_fut in
      Console.log
        [ "Get all keys "; Performance.now_ms G.performance -. n; " ms" ];
      let n = Performance.now_ms G.performance in
      let keys =
        match src_views with
        | All -> all_keys
        | One_of src_views ->
            Array.filter all_keys
              ~f:(fun { Db.Generic_schema.Track.Key.collections; _ } ->
                List.exists collections ~f:(fun v -> List.memq v ~set:src_views))
        | None_of _ -> failwith "not implemented"
      in
      let keys =
        Array.filter keys
          ~f:(fun
              {
                Db.Generic_schema.Track.Key.name;
                Db.Generic_schema.Track.Key.genres;
                Db.Generic_schema.Track.Key.artists;
                Db.Generic_schema.Track.Key.album_artists;
                _;
              }
            ->
            let genres = Int.Set.of_list genres in
            let artists =
              let track = Int.Set.of_list artists in
              Int.Set.add_list track album_artists
            in
            List.fold_left filters ~init:true ~f:(fun acc -> function
              | Db.View.Search "" -> true
              | Search sub ->
                  let sub = String.lowercase_ascii sub in
                  let pattern = String.Find.compile (Printf.sprintf "%s" sub) in
                  let name = String.lowercase_ascii name in
                  acc && String.Find.find ~pattern name >= 0
              | Genres filter -> acc && match_filter ~filter genres
              | Artists filter -> acc && match_filter ~filter artists))
      in
      Console.log
        [ "Filter took "; Performance.now_ms G.performance -. n; " ms" ];
      let n = Performance.now_ms G.performance in
      let () =
        match sort with
        | Name ->
            (* TODO sort should be achieved by using the sort_name index*)
            Array.sort keys
              ~cmp:(fun
                  { Db.Generic_schema.Track.Key.name = sna; _ }
                  { Db.Generic_schema.Track.Key.name = snb; _ }
                -> String.compare sna snb)
        | _ -> ()
      in
      Console.log [ "Sort took "; Performance.now_ms G.performance -. n; " ms" ];
      Hashtbl.add view_memo (Db.View.hash req) keys;
      keys

  (* TODO there is no reason to delegate everything to the worker, only view
     creation is really slow *)
  let on_query (type a) (q : a query) : (a, error) Fut.result =
    let open Fut.Result_syntax in
    match q with
    | Set_session_uuid s ->
        let () = Data_source.Jellyfin_api.set_session_uuid s in
        Fut.ok ()
    | Add_servers l ->
        let* idb = idb in
        let open Fut.Syntax in
        let+ res = check_db idb (List.hd l) in
        Result.map_err (fun jv -> `Jv jv) res
    | Get_libraries () ->
        let* store = get_store (module Db.Stores.Collections_store) () in
        let keys =
          Db.Stores.Collections_store.get_all_keys store |> IDB.Request.fut
        in
        let records =
          Db.Stores.Collections_store.get_all store |> IDB.Request.fut
        in
        Fut.pair keys records
        |> Fut.map (function
             | Ok keys, Ok records ->
                 Ok (Array.map2 ~f:(fun k r -> (k, r)) keys records)
             | Error e, Ok _ | _, Error e -> Error (`Jv e))
    | Create_view request ->
        let* store = get_store (module Db.Stores.Tracks_store) () in
        let+ keys = get_view_keys store request in
        let item_count = Array.length keys in
        let duration =
          Array.fold_left keys ~init:0.
            ~f:(fun acc { Db.Generic_schema.Track.Key.duration; _ } ->
              acc +. duration)
        in
        { Db.View.request; start_offset = 0; item_count; duration }
    | Get_view_genres view ->
        let* store = get_store (module Tracks_store) () in
        let* keys = get_view_keys store view.request in
        let* s_genres = get_store (module Genres_store) () in
        let+ genres = Genres_store.get_all s_genres |> as_fut in
        Array.fold_left keys ~init:Int.Map.empty
          ~f:(fun acc { Db.Generic_schema.Track.Key.genres; _ } ->
            Int.Map.add_list_with
              ~f:(fun _ -> ( + ))
              acc
              (List.map genres ~f:(fun g -> (g, 1))))
        |> Int.Map.mapi (fun key usage_count ->
               try
                 (usage_count, genres.(key - 1))
                 (* Indexeddb auto increments starts at 1 *)
               with Invalid_argument _ -> failwith "Unknown genre")
    | Get_view_artists view ->
        let* store = get_store (module Tracks_store) () in
        let* keys = get_view_keys store view.request in
        let* s_artists = get_store (module Artists_store) () in
        let+ artists = Artists_store.get_all s_artists |> as_fut in
        Array.fold_left keys ~init:Int.Map.empty
          ~f:(fun
              acc { Db.Generic_schema.Track.Key.artists; album_artists; _ } ->
            let artists = List.rev_append album_artists artists in
            Int.Map.add_list_with
              ~f:(fun _ -> ( + ))
              acc
              (List.map artists ~f:(fun g -> (g, 1))))
        |> Int.Map.mapi (fun key count ->
               try
                 { Db.Generic_schema.count; v = artists.(key - 1) }
                 (* Indexeddb auto increments starts at 1 *)
               with Invalid_argument _ -> failwith "Unknown genre")
    | Get (view, order, indexes) ->
        (* This request is critical to virtual lists performances and should
           be as fast as possible. *)
        let* store = get_store (module Db.Stores.Tracks_store) () in
        let* keys = get_view_keys store view.request in
        let open Fut.Syntax in
        let+ results =
          Array.map indexes ~f:(fun index ->
              try
                let index = index + view.start_offset in
                let index =
                  Db.View.Order.apply ~size:view.item_count order index
                in
                (* This could be optimize when access is sequential *)
                let key = keys.(index) in
                let open Fut.Syntax in
                let+ result =
                  Db.Stores.Tracks_store.get key store |> IDB.Request.fut
                in
                match result with
                | Ok None -> None
                | Error err ->
                    Console.error
                      [ "An error occured while loading item"; key; err ];
                    None
                | Ok (Some v) -> Some (key, v)
              with _ -> Fut.return None)
          |> fut_of_array
        in
        Ok results
end

include Make_worker (Worker)

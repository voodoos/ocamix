open! Std
module Int_set = Set.Make (Int)
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
  let view_memo :
      ( int Db.View.selection * Db.View.Sort.t,
        Db.Generic_schema.Track.Key.t array )
      Hashtbl.t =
    Hashtbl.create 64

  let last_view : (int * Db.Generic_schema.Track.Key.t array) ref =
    ref (-1, [||])

  let check_db idb source =
    let server_id, source = source in
    let report status =
      Hashtbl.clear view_memo;
      last_view := (-1, [||]);
      dispatch_event Servers_status_update (server_id, status)
    in
    Db.Sync.check_and_sync ~report ~source idb

  let idb =
    let idb, set_idb = Fut.create () in
    let _ =
      Db.with_idb ~name:"tracks" ~version:1 @@ fun idb ->
      ignore (set_idb @@ Ok idb)
    in
    idb

  let get_store (type t') (module Store : IDB.Store_intf with type t = t')
      ?(mode = IDB.Transaction.Readonly) () : (t', error) Fut.result =
    let open Fut.Result_syntax in
    let+ idb = idb in
    IDB.Database.transaction [ (module Store) ] ~mode idb
    |> IDB.Transaction.object_store (module Store)

  let get_view_keys store { Db.View.kind = _; src_views; sort; filters } =
    (* todo: staged memoization + specialized queries using indexes *)
    let open Fut.Result_syntax in
    let n = Performance.now_ms G.performance in
    let+ keys =
      let+ all_keys = Db.Stores.Tracks_store.get_all_keys store |> as_fut in
      Console.log
        [ "Get all keys "; Performance.now_ms G.performance -. n; " ms" ];
      let n = Performance.now_ms G.performance in
      let keys =
        match src_views with
        | All -> all_keys
        | Only src_views ->
            Array.filter all_keys
              ~f:(fun { Db.Generic_schema.Track.Key.collections; _ } ->
                List.exists collections ~f:(fun v -> List.memq v ~set:src_views))
      in
      Console.log
        [ "Filter took "; Performance.now_ms G.performance -. n; " ms" ];
      keys
    in
    let keys =
      match filters with
      | [ Search sub ] when not (String.is_empty sub) ->
          let sub = String.lowercase_ascii sub in
          let pattern = String.Find.compile (Printf.sprintf "%s" sub) in
          Array.filter keys ~f:(fun { Db.Generic_schema.Track.Key.name; _ } ->
              let name = String.lowercase_ascii name in
              String.Find.find ~pattern name >= 0)
      | _ -> keys
    in
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
        { Db.View.request; start_offset = 0; item_count }
    | Get_view_albums view ->
        let* store = get_store (module Tracks_store) () in
        let* keys = get_view_keys store view.request in
        let* s_genres = get_store (module Genres_store) () in
        let+ genres = Genres_store.get_all s_genres |> as_fut in
        Array.fold_left keys ~init:Int_set.empty
          ~f:(fun acc { Db.Generic_schema.Track.Key.genres; _ } ->
            Int_set.add_list acc genres)
        |> Int_set.to_list
        |> List.map ~f:(fun key ->
               try { Db.Generic_schema.key; value = genres.(key - 1) }
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

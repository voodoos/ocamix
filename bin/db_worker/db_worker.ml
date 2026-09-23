open Db.Worker_api
open! Std
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
  let view_memo : (int, Tracks_store.Primary_key.t array) Hashtbl.t =
    Hashtbl.create 64

  let view_memo_order : int Queue.t = Queue.create ()
  let view_memo_max = 64

  let memoize_view hash keys =
    if not (Hashtbl.mem view_memo hash) then begin
      if Queue.length view_memo_order >= view_memo_max then
        Option.iter (Hashtbl.remove view_memo) (Queue.take_opt view_memo_order);
      Queue.add hash view_memo_order;
      Hashtbl.add view_memo hash keys
    end

  let invalidate_cache () =
    Hashtbl.reset view_memo;
    Queue.clear view_memo_order

  let check_db idb source =
    let server_id, source = source in
    let report status =
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

  (* The filters are compiled once for the whole scan, instead of being
     interpreted again for each of the library's tracks. A [None] field means
     "accepts everything". *)
  type compiled_filters = {
    collections : (int list -> bool) option;
    name : (string -> bool) option;
    genres : (Int.Set.t -> bool) option;
    artists : (Int.Set.t -> bool) option;
  }

  let no_filter =
    { collections = None; name = None; genres = None; artists = None }

  (* Conjunction, so that repeating a filter kind keeps the [&&] semantics. *)
  let both current f =
    match current with
    | None -> Some f
    | Some current -> Some (fun x -> current x && f x)

  let compile_filters ~src_views filters =
    (* A selection made only of [All] accepts everything. *)
    let accepts_all = List.for_all ~f:(Equal.poly Db.View.Selection.All) in
    let collections =
      match src_views with
      | Db.View.Selection.All -> None
      | One_of src_views ->
          Some
            (fun collections ->
              List.exists collections ~f:(fun v -> List.memq v ~set:src_views))
      | None_of _ -> failwith "not implemented"
    in
    List.fold_left filters ~init:{ no_filter with collections }
      ~f:(fun acc -> function
      | Db.View.Search "" -> acc
      | Search sub ->
          let pattern = String.Find.compile (String.lowercase_ascii sub) in
          let matches name =
            String.Find.find ~pattern (String.lowercase_ascii name) >= 0
          in
          { acc with name = both acc.name matches }
      | Genres filter when accepts_all filter -> acc
      | Genres filter ->
          { acc with genres = both acc.genres (match_filter ~filter) }
      | Artists filter when accepts_all filter -> acc
      | Artists filter ->
          { acc with artists = both acc.artists (match_filter ~filter) })

  let keep filters
      {
        Db.Generic_schema.Track.Key.name;
        genres;
        artists;
        album_artists;
        collections;
        _;
      } =
    let test field value =
      match field with None -> true | Some f -> f value
    in
    test filters.collections collections
    && test filters.name name
    && (match filters.genres with
      | None -> true
      | Some f -> f (Int.Set.of_list genres))
    &&
    match filters.artists with
    | None -> true
    | Some f -> f (Int.Set.add_list (Int.Set.of_list artists) album_artists)

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
      let filters = compile_filters ~src_views filters in
      let keys = Array.filter all_keys ~f:(keep filters) in
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
        | Date_added ->
            Array.sort keys
              ~cmp:(fun
                  { Db.Generic_schema.Track.Key.date_created = sna; _ }
                  { Db.Generic_schema.Track.Key.date_created = snb; _ }
                -> String.compare sna snb)
      in
      Console.log [ "Sort took "; Performance.now_ms G.performance -. n; " ms" ];
      memoize_view (Db.View.hash req) keys;
      keys

  (* TODO there is no reason to delegate everything to the worker, only view
     creation is really slow *)
  let on_query (type a b) (q : (a, b) query) (params : a) :
      (b, error) Fut.result =
    let open Fut.Result_syntax in
    match q with
    | Set_session_uuid ->
        let () = Data_source.Jellyfin_api.set_session_uuid params in
        Fut.ok ()
    | Add_servers ->
        let* idb = idb in
        let open Fut.Syntax in
        let+ res = check_db idb (List.hd params) in
        Result.map_err (fun jv -> `Jv jv) res
    | Get_libraries ->
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
    | Create_view ->
        let request = params in
        let* store = get_store (module Db.Stores.Tracks_store) () in
        let+ keys = get_view_keys store request in
        let item_count = Array.length keys in
        let duration =
          Array.fold_left keys ~init:0.
            ~f:(fun acc { Db.Generic_schema.Track.Key.duration; _ } ->
              acc +. duration)
        in
        { Db.View.request; start_offset = 0; item_count; duration }
    | Get_view_genres ->
        let* store = get_store (module Tracks_store) () in
        let* keys = get_view_keys store params.request in
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
    | Get_view_artists ->
        let* store = get_store (module Tracks_store) () in
        let* keys = get_view_keys store params.request in
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
    | Get_tracks ->
        (* This request is critical to virtual lists performances and should
           be as fast as possible. *)
        let view, indexes = params in
        let* keys =
          let* store = get_store (module Tracks_store) () in
          get_view_keys store view.request
        in
        let* idb = idb in
        let transaction =
          IDB.Database.transaction
            [ (module Tracks_store); (module Albums_store) ]
            ~mode:Readonly idb
        in
        let store, album_store =
          ( IDB.Transaction.object_store (module Tracks_store) transaction,
            IDB.Transaction.object_store (module Albums_store) transaction )
        in
        let albums = Hashtbl.create 64 in
        let get_album album_id =
          match Hashtbl.get albums album_id with
          | Some album -> album
          | None ->
              let open Fut.Syntax in
              let album =
                let+ result =
                  Albums_store.get album_id album_store |> IDB.Request.fut
                in
                match result with
                | Ok album -> album
                | Error err ->
                    Console.error [ "Could not load album"; album_id; err ];
                    None
              in
              Hashtbl.add albums album_id album;
              album
        in
        let open Fut.Syntax in
        let+ results =
          Array.map indexes ~f:(fun index ->
              (* The view may have shrunk since the table asked for this row. *)
              if index < 0 || index >= Array.length keys then Fut.return None
              else
                let key = keys.(index) in
                let* result = Tracks_store.get key store |> IDB.Request.fut in
                match result with
                | Ok None -> Fut.return None
                | Error err ->
                    Console.error
                      [ "An error occured while loading item"; key; err ];
                    Fut.return None
                | Ok (Some v) ->
                    let+ album =
                      match v.album_id with
                      | None -> Fut.return None
                      | Some album_id -> get_album album_id
                    in
                    Some (key, v, album))
          |> fut_of_array
        in
        Ok results
end

include Make_worker (Worker)

open! Std
open Db.Worker_api
open Brrer
open! Brr
module IDB = Brr_io.Indexed_db
module IS = Db.Item_store

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
      ( string Db.View.selection * Db.View.Sort.t,
        IS.Content.Key.t array )
      Hashtbl.t =
    Hashtbl.create 64

  let last_view : (int * IS.Content.Key.t array) ref = ref (-1, [||])

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

  let read_only_store () =
    let open Fut.Result_syntax in
    let+ idb = idb in
    IDB.Database.transaction [ (module Db.I) ] ~mode:Readonly idb
    |> IDB.Transaction.object_store (module Db.I)

  let get_view_keys store { Db.View.kind = _; src_views; sort; filters } =
    (* todo: staged memoization + specialized queries using indexes *)
    let open Fut.Result_syntax in
    let hash = Hashtbl.hash (src_views, sort, filters) in
    if Int.equal (fst !last_view) hash then Fut.ok (snd !last_view)
    else
      let+ keys =
        try Fut.ok @@ Hashtbl.find view_memo (src_views, sort)
        with Not_found ->
          let+ all_keys =
            let lower = Jv.of_array Jv.of_string [| "Audio" |] in
            let upper = Jv.of_array Jv.of_string [| "Audio\u{0}" |] in
            let query =
              IDB.Key_range.bound ~lower ~upper ~lower_open:true
                ~upper_open:false ()
            in
            let idx = IS.index (module IS.Index.Kind_View) store in
            IS.Index.Kind_View.get_all_keys ~query idx |> as_fut
          in
          let keys =
            match src_views with
            | All -> all_keys
            | Only src_views ->
                Array.filter all_keys
                  ~f:(fun { Db.Stores.Items.Key.views; _ } ->
                    List.exists views ~f:(fun v -> List.memq v ~set:src_views))
          in
          Hashtbl.add view_memo (src_views, sort) keys;
          keys
      in
      let keys =
        match filters with
        | [ Search sub ] when not (String.is_empty sub) ->
            let sub = String.lowercase_ascii sub in
            let pattern = String.Find.compile (Printf.sprintf "%s" sub) in
            Array.filter keys ~f:(fun { Db.Stores.Items.Key.sort_name; _ } ->
                let sort_name = String.lowercase_ascii sort_name in
                String.Find.find ~pattern sort_name >= 0)
        | _ -> keys
      in
      let () =
        match sort with
        | Name ->
            Array.sort keys
              ~cmp:(fun
                  { Db.Stores.Items.Key.sort_name = sna; _ }
                  { Db.Stores.Items.Key.sort_name = snb; _ }
                -> String.compare sna snb)
        | _ -> ()
      in
      last_view := (hash, keys);
      keys

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
    | Get_all () ->
        let* store = read_only_store () in
        let+ req = Db.I.get_all store |> as_fut in
        Array.map ~f:(fun i -> i.Db.Stores.Items.item) req |> Array.to_list
    | Get_server_libraries server_id' ->
        let* store = read_only_store () in
        let index = IS.(index (module IS.Index.Type_Name) store) in
        let lower = Jv.of_array Jv.of_string [| "music" |] in
        let upper = Jv.of_array Jv.of_string [| "music\u{0}" |] in
        let query =
          IDB.Key_range.bound ~lower ~upper ~lower_open:true ~upper_open:false
            ()
        in
        let* keys = IS.Index.Type_Name.get_all_keys ~query index |> as_fut in
        let open Fut.Syntax in
        let+ items =
          List.map (Array.to_list keys) ~f:(fun k -> IS.get k store |> as_fut)
          |> Fut.of_list
        in
        let items =
          Result.flatten_l items
          |> Result.map
               (List.filter_map ~f:(function
                 | Some ({ Db.Stores.Items.item = { server_id; _ }; _ } as item)
                   when String.equal server_id server_id' ->
                     Some item
                 | Some _ | None -> None))
        in
        items
    | Get_libraries () ->
        let* store = read_only_store () in
        let index = IS.(index (module IS.Index.Type_Name) store) in
        let lower = Jv.of_array Jv.of_string [| "music" |] in
        let upper = Jv.of_array Jv.of_string [| "music\u{0}" |] in
        let query =
          IDB.Key_range.bound ~lower ~upper ~lower_open:true ~upper_open:false
            ()
        in
        let* keys = IS.Index.Type_Name.get_all_keys ~query index |> as_fut in
        let open Fut.Syntax in
        let+ items =
          List.map (Array.to_list keys) ~f:(fun k -> IS.get k store |> as_fut)
          |> Fut.of_list
        in
        let items =
          Result.flatten_l items
          |> Result.map (fun l ->
                 List.map l ~f:(Option.get_exn_or "Item should exists."))
        in
        items
    | Create_view request ->
        let* store = read_only_store () in
        let+ keys = get_view_keys store request in
        let item_count = Array.length keys in
        { Db.View.request; start_offset = 0; item_count }
    | Get (view, order, indexes) ->
        (* This request is critical to virtual lists performances and should
           be as fast as possible. *)
        let* store = read_only_store () in
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
                let+ result = IS.get key store |> IDB.Request.fut in
                match result with
                | Ok None -> None
                | Error err ->
                    Console.error
                      [ "An error occured while loading item"; key; err ];
                    None
                | Ok (Some v) -> Some v
              with _ -> Fut.return None)
          |> fut_of_array
        in
        Ok results
end

include Make_worker (Worker)

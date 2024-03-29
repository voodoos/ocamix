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
  let check_db idb source =
    let server_id, source = source in
    let report status =
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

  let get_view_keys =
    let view_memo : (string Db.View.selection, IS.Content.Key.t array) Hashtbl.t
        =
      Hashtbl.create 64
    in
    fun store { Db.View.kind = _; src_views; sort = _; _ } ->
      (* todo: staged memoization + specialized queries using indexes *)
      let open Fut.Result_syntax in
      try Fut.ok @@ Hashtbl.find view_memo src_views
      with Not_found ->
        Console.debug [ "View not found" ];
        let f = Performance.now_ms G.performance in
        let idx = IS.index (module IS.Index.Kind_View) store in
        let+ all_keys =
          let lower = Jv.of_array Jv.of_string [| "Audio" |] in
          let upper = Jv.of_array Jv.of_string [| "Audio\u{0}" |] in
          let query =
            IDB.Key_range.bound ~lower ~upper ~lower_open:true ~upper_open:false
              ()
          in
          IS.Index.Kind_View.get_all_keys ~query idx |> as_fut
        in
        let keys =
          match src_views with
          | All -> all_keys
          | Only src_views ->
              Array.filter all_keys ~f:(fun (_, _sn, views) ->
                  List.exists views ~f:(fun v -> List.memq v ~set:src_views))
        in
        let f' = Performance.now_ms G.performance in
        Console.log [ "Uncached view creation took:"; f' -. f; "ms" ];
        Hashtbl.add view_memo src_views keys;
        keys

  let on_query (type a) (q : a query) : (a, error) Fut.result =
    let open Fut.Result_syntax in
    match q with
    | Add_servers l ->
        let* idb = idb in
        let open Fut.Syntax in
        let+ res = check_db idb (List.hd l) in
        Result.map_err (fun jv -> `Jv jv) res
    | Get_all () ->
        let* store = read_only_store () in
        let+ req = Db.I.get_all store |> as_fut in
        Array.map ~f:(fun i -> i.Db.Stores.Items.item) req |> Array.to_list
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
        let uuid = new_uuid_v4 () in
        let* store = read_only_store () in
        let+ keys = get_view_keys store request in
        let item_count = Array.length keys in
        let order = Db.View.Order.of_sort ~size:item_count request.sort in
        { Db.View.uuid; request; order; start_offset = 0; item_count }
    | Get (view, indexes) ->
        (* This request is critical to virtual lists performances and should
           be as fast as possible. *)
        let* store = read_only_store () in
        let* keys = get_view_keys store view.request in
        let open Fut.Syntax in
        let+ results =
          Array.map indexes ~f:(fun index ->
              try
                let index = index + view.start_offset in
                let index = Db.View.Order.apply view.order index in
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

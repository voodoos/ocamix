open! Std
open Db.Worker_api
open Brrer
module IDB = Brr_io.Indexed_db
module IS = Db.Item_store

let () = Random.self_init ()

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
        let+ req = Db.I.get_all store |> IDB.Request.fut in
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
        let* keys =
          IS.Index.Type_Name.get_all_keys ~query index |> IDB.Request.fut
        in
        let open Fut.Syntax in
        let+ items =
          List.map (Array.to_list keys) ~f:(fun k ->
              IS.get k store |> IDB.Request.fut)
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
        let _ =
          let f = Brr.Performance.now_ms Brr.G.performance in
          let+ arr = IS.get_all_keys store |> IDB.Request.fut in
          Brr.Console.log
            [ "allkeys took"; Brr.Performance.now_ms Brr.G.performance -. f ];
          let f = Brr.Performance.now_ms Brr.G.performance in
          let () =
            Array.fast_sort arr ~cmp:(fun (_, k1, _) (_, k2, _) ->
                String.compare k1 k2)
          in
          Brr.Console.log
            [ "sorting took"; Brr.Performance.now_ms Brr.G.performance -. f ];
          let f = Brr.Performance.now_ms Brr.G.performance in

          let arr =
            Array.filter ~f:(fun (_, k, _) -> String.prefix ~pre:"0" k) arr
          in
          Brr.Console.log
            [
              "filtering took";
              Brr.Performance.now_ms Brr.G.performance -. f;
              arr;
            ];
          let f = Brr.Performance.now_ms Brr.G.performance in
          let+ _ =
            Db.I.fold_keys ~init:[] ~f:(fun acc key _ -> key :: acc)
            @@ Db.I.open_key_cursor store
          in
          Brr.Console.log
            [
              "allkeys_fold took"; Brr.Performance.now_ms Brr.G.performance -. f;
            ]
        in
        let+ item_count = Db.I.count () store |> IDB.Request.fut in
        let order = Db.View.Order.of_sort ~size:item_count request.sort in
        { Db.View.uuid; request; order; start_offset = 0; item_count }
    | Get (view, index) -> (
        let index = index + view.start_offset in
        let index = Db.View.Order.apply view.order index in
        let* store = read_only_store () in
        let idx = Db.I.index (module Db.Stores.ItemsByDateAdded) store in
        let* res =
          Db.Stores.ItemsByDateAdded.get index idx |> IDB.Request.fut
        in
        match res with
        | Some res -> Fut.ok res
        | None -> Fut.return (Error (`Msg "Item not found")))
end

include Make_worker (Worker)

open! Std
open Db.Worker_api
open Brrer
module IDB = Brr_io.Indexed_db

let () = Random.self_init ()

module Worker () = struct
  let source, set_source = Fut.create ()

  let check_db idb =
    let open Fut.Result_syntax in
    let* _, source = source in
    Db.Sync.check_and_sync ~source idb

  let idb =
    let idb, set_idb = Fut.create () in
    let _ =
      let open Fut.Result_syntax in
      Db.with_idb ~name:"tracks" ~version:1 @@ fun idb ->
      ignore
        (let+ () = check_db idb in
         set_idb @@ Ok idb)
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
    | Servers l ->
        set_source @@ Ok (List.hd l);
        Fut.ok ()
    | Get_all () ->
        let* idb = idb in
        let store =
          IDB.Database.transaction [ (module Db.I) ] ~mode:Readonly idb
          |> IDB.Transaction.object_store (module Db.I)
        in
        let+ req = Db.I.get_all store |> IDB.Request.fut in
        Array.map ~f:(fun i -> i.Db.Stores.Items.item) req |> Array.to_list
    | Create_view request ->
        let uuid = new_uuid_v4 () in
        let* store = read_only_store () in
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

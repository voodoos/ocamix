open! Std
open Db.Worker_api
open Brrer
module IDB = Brr_io.Indexed_db

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
    | Create_view () ->
        let uuid = new_uuid_v4 () in

        Fut.ok { View.uuid; item_count = 100 }
    | Get index ->
        let* idb = idb in
        let store =
          IDB.Database.transaction [ (module Db.I) ] ~mode:Readonly idb
          |> IDB.Transaction.object_store (module Db.I)
        in
        let idx = Db.I.index (module Db.Stores.ItemsByDateAdded) store in
        let v, set = Fut.create () in
        ignore
          (Db.Stores.ItemsByDateAdded.get index idx
          |> IDB.Request.on_success ~f:(fun _ r ->
                 Option.iter (fun t -> set (Ok t)) (IDB.Request.result r)));
        v
end

include Make_worker (Worker)

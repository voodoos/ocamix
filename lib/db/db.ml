module Stores = Stores
module Sync = Sync
open Brrer
open Brr
module OI = Stores.Orderred_items_store
module I = Stores.Items_store

let with_idb ?version ~name f =
  let open Brr_io.Indexed_db in
  let f _ev dbr =
    let db = Request.result dbr in
    f db
  in
  get_factory ()
  |> Factory.open' ~name ?version
  |> Open_db_request.on_upgrade_needed ~f:(fun e q ->
         let old_version, new_version =
           let v = Ev.as_type e in
           Events.Version_change.(old_version v, new_version v)
         in
         Console.info
           [
             "Upgrading indexed_db schema from version";
             old_version;
             "to";
             new_version;
           ];
         let db = Request.result q in
         let list =
           Database.create_object_store (module OI) ~auto_increment:false db
         in
         let items =
           Database.create_object_store (module I) ~auto_increment:false db
         in
         Console.info [ "Stores created:"; list; items ])
  |> Request.on_success ~f |> ignore

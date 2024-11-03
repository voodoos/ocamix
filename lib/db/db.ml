module Stores = Stores
module Sync = Sync
module View = View
open Brrer
open Brr
open Brr_io
module Worker_api = Db_worker_api

module Item_store = struct
  include Stores.Items_store

  module Index = struct
    module Id = Stores.ItemsById
    module Date_added = Stores.ItemsByDateAdded
    module Type_Name = Stores.ItemsByTypeAndName
    module Kind_View = Stores.ItemsByViewAndKind
  end
end

open Stores

let on_upgrade_needed e q =
  let open Brr_io.Indexed_db in
  let old_version, new_version =
    let v = Ev.as_type e in
    Events.Version_change.(old_version v, new_version v)
  in
  Console.info
    [
      "Upgrading indexed_db schema from version"; old_version; "to"; new_version;
    ];
  let db = Request.result q in
  let stores = Database.object_store_names db in
  Console.info [ "Erasing existing stores" ];
  Array.iter (Database.delete_object_store db) stores;
  let list =
    let key_path = Indexed_db.Key_path.Identifier "id" in
    Orderred_items_store.create db ~key_path ~auto_increment:false
  in
  let _ = ItemsByDateAdded.create list in
  let items =
    let key_path =
      Indexed_db.Key_path.(
        Identifiers [| "item.Id"; "item.Name"; "sorts.views" |])
    in
    Item_store.create ~key_path ~auto_increment:false db
  in
  let () =
    ignore
      ( ItemsByTypeAndName.create items,
        ItemsByViewAndKind.create items,
        ItemsById.create items )
  in
  let virtual_folders =
    let key_path = Indexed_db.Key_path.Identifier "ItemId" in
    Virtual_folder_store.create ~key_path ~auto_increment:false db
  in
  Console.info [ "Stores created:"; list; items; virtual_folders ]

let with_idb ?version ~name f =
  let open Brr_io.Indexed_db in
  let f _ev dbr =
    let db = Request.result dbr in
    f db
  in
  get_factory ()
  |> Factory.open' ~name ?version
  |> Open_db_request.on_upgrade_needed ~f:on_upgrade_needed
  |> Request.on_success ~f |> ignore

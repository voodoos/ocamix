module Stores = Stores
module Sync = Sync
module View = View
open Brrer
open Brr
open Brr_io
module OI = Stores.Orderred_items_store
module I = Stores.Items_store
module VF = Stores.Virtual_folder_store
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
    Database.create_object_store (module OI) ~key_path ~auto_increment:false db
  in
  let items =
    let key_path =
      Indexed_db.Key_path.(
        Identifiers [| "item.Id"; "item.Name"; "sorts.views" |])
    in
    Database.create_object_store (module I) ~key_path ~auto_increment:false db
  in
  let virtual_folders =
    let key_path = Indexed_db.Key_path.Identifier "ItemId" in
    Database.create_object_store (module VF) ~key_path ~auto_increment:false db
  in
  let open Stores in
  let _genres =
    Database.create_object_store (module Genres_store) ~auto_increment:true db
  in
  let index_date_added = I.create_index (module ItemsByDateAdded) items in
  let _ = I.create_index (module ItemsByTypeAndName) items in
  let _ = I.create_index (module ItemsByViewAndKind) items in
  let _ = I.create_index (module ItemsById) items in
  Console.info
    [ "Stores created:"; list; items; index_date_added; virtual_folders ]

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

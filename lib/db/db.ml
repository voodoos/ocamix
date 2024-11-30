module Stores = Stores
module Sync = Sync
module View = View
open Brrer
open Brr
open Brr_io
module Worker_api = Db_worker_api
module Generic_schema = Generic_schema

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
  let _ =
    ItemsByDateAdded.create ~name:"items_by_date_added"
      (Key_path.Identifier "sorts.date_added") list
  in
  let items =
    let key_path =
      Indexed_db.Key_path.(
        Identifiers [| "item.Id"; "item.Name"; "sorts.views" |])
    in
    Item_store.create ~key_path ~auto_increment:false db
  in
  let () =
    ignore
      ( ItemsByTypeAndName.create ~name:"items_by_type_and_name"
          (Key_path.Identifiers [| "item.CollectionType"; "sorts.sort_name" |])
          items,
        ItemsByViewAndKind.create ~name:"items_by_view_and_kind"
          (Key_path.Identifiers [| "item.Type"; "sorts.views" |])
          items,
        ItemsById.create ~name:"items_by_id" (Key_path.Identifier "item.Id")
          items )
  in
  let virtual_folders =
    let key_path = Indexed_db.Key_path.Identifier "ItemId" in
    Virtual_folder_store.create ~key_path ~auto_increment:false db
  in
  let _collections =
    Collections_store.create ~auto_increment:true db
    |> Collections_by_id.create ~name:"by-id" ~unique:true
         (Key_path.Identifier "id")
  in
  let _genres =
    Genres_store.create ~auto_increment:true db
    |> Genres_by_canonical_name.create ~name:"genres_by_canon_name" ~unique:true
         (Key_path.Identifier "canon")
  in
  let _artists =
    let store = Artists_store.create ~auto_increment:true db in
    Artists_by_id.create ~name:"by-id" (Key_path.Identifier "id") ~unique:true
      store
    |> ignore;
    Artists_by_id.create ~name:"by-mbid" (Key_path.Identifier "mbid")
      ~unique:true store
  in
  let _albums =
    let store = Albums_store.create db in
    Albums_by_id.create ~name:"by-id" (Key_path.Identifier "id") ~unique:true
      store
    |> ignore;
    Albums_by_idx.create ~name:"by-idx" (Key_path.Identifier "idx") store
    |> ignore
  in
  let _tracks = Tracks_store.create db in
  Console.info [ "Stores created:"; list; items; virtual_folders ]

let schema_version = 2

let with_idb ?(version = schema_version) ?(name = "tracks") f =
  let open Brr_io.Indexed_db in
  let f _ev dbr =
    let db = Request.result dbr in
    f db
  in
  get_factory ()
  |> Factory.open' ~name ~version
  |> Open_db_request.on_upgrade_needed ~f:on_upgrade_needed
  |> Request.on_success ~f |> ignore

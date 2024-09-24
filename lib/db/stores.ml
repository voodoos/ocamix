open! Std
open Brrer
open Brr_io
module Api = Data_source.Jellyfin_api

(* Proposition:
   - Use primary key to provide filtering
   - Use index keys to provide sorts

   Big primary keys drive the cost of [get_all_keys] up.
   We could delegate some filtering to the index keys if necessary.

    We could also tried to pre-process or compress the keys *)

let t_to_jv encoder t =
  encoder t |> Yojson.Safe.to_string |> Jstr.of_string |> Brr.Json.decode
  |> Result.get_exn

let jv_to_t decoder j =
  let json = Brr.Json.encode j in
  Ok (Jstr.to_string json |> Yojson.Safe.from_string |> decoder)

module Orderred_items = struct
  type t = { id : int; item : string option } [@@deriving yojson]

  module Key = struct
    type t = int

    let to_jv k = Jv.of_int k
    let of_jv j = Jv.to_int j
    let path = Indexed_db.Key_path.Id "id"
  end

  let name = "items_by_date_added"

  let to_jv t =
    let obj =
      Jv.obj
        [|
          ("id", Jv.of_int t.id);
          ("item", Jv.of_option ~none:(Jv.of_string "") Jv.of_string t.item);
        |]
    in
    obj

  let of_jv j =
    {
      id = Jv.get j "id" |> Jv.to_int;
      item = Jv.get j "item" |> Jv.to_option Jv.to_string;
    }

  let get_key t = t.id
end

module Items = struct
  open Data_source.Jellyfin.Api

  type sorts = { date_added : int; views : string list; sort_name : string }
  [@@deriving yojson]

  type t = { sorts : sorts; item : Item.t } [@@deriving yojson]

  let compare t t' = String.compare t.sorts.sort_name t'.sorts.sort_name

  module Key = struct
    type t = { id : string; sort_name : string; views : string list }

    let to_jv { id; sort_name; views } =
      let id = Jv.of_string id in
      let sort_name = Jv.of_string sort_name in
      let views = Jv.of_list Jv.of_string views in
      Jv.of_jv_array [| id; sort_name; views |]

    let of_jv j =
      match Jv.to_jv_array j with
      | [| id; sort_name; views |] ->
          let id = Jv.to_string id in
          let sort_name = Jv.to_string sort_name in
          let views = Jv.to_list Jv.to_string views in
          { id; sort_name; views }
      | _ -> assert false

    let path =
      Indexed_db.Key_path.(
        S [| Id "item.Id"; Id "item.Name"; Id "sorts.views" |])
  end

  module Key_date_added = struct
    type t = int

    let to_jv k = Jv.of_int k
    let of_jv j = Jv.to_int j
    let path = Indexed_db.Key_path.Id "sorts.date_added"
  end

  module Key_id = struct
    type t = string

    let to_jv k = Jv.of_string k
    let of_jv j = Jv.to_string j
    let path = Indexed_db.Key_path.Id "item.Id"
  end

  module Key_view_kind = struct
    (* todo: use a enum for kinds *)
    type t = { type' : string; views : string list }

    let to_jv _k = assert false

    let of_jv j =
      match Jv.(to_jv_array j) with
      | [| type'; views |] ->
          { type' = Jv.to_string type'; views = Jv.(to_list to_string views) }
      | _ -> assert false

    let path = Indexed_db.Key_path.S [| Id "item.Type"; Id "sorts.views" |]
  end

  module Key_type_name = struct
    type t = { collection_type : string; sort_name : string }

    let to_jv _t = assert false

    let of_jv j =
      match Jv.(to_jv_array j) with
      | [| collection_type; sort_name |] ->
          {
            collection_type = Jv.to_string collection_type;
            sort_name = Jv.to_string sort_name;
          }
      | _ -> assert false

    let path =
      Indexed_db.Key_path.S [| Id "item.CollectionType"; Id "sorts.sort_name" |]
  end

  let name = "items"
  let to_jv t = t_to_jv yojson_of_t t
  let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn

  let get_key t =
    {
      Key.sort_name = t.sorts.sort_name;
      id = t.item.Item.id;
      views = t.sorts.views;
    }
end

module Virtual_folder = struct
  open Data_source.Jellyfin_api

  (* todo: multiserver: we should add a server_id key *)
  type t = Virtual_folders.virtual_folder [@@deriving yojson]

  module Key = struct
    type t = string

    let to_jv k = Jv.of_string k
    let of_jv j = Jv.to_string j
    let path = Indexed_db.Key_path.Id "ItemId"
  end

  let name = "virtual_folders"
  let to_jv t = t_to_jv yojson_of_t t
  let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
  let get_key t = t.Virtual_folders.item_id
end

module Orderred_items_store = Indexed_db.Make_object_store (Orderred_items)
module Items_store = Indexed_db.Make_object_store (Items)
module Virtual_folder_store = Indexed_db.Make_object_store (Virtual_folder)

module ItemsByDateAdded =
  Indexed_db.Make_index
    (struct
      let name = "items_by_date_added"
    end)
    (Items)
    (Items.Key_date_added)

module ItemsByViewAndKind =
  Indexed_db.Make_index
    (struct
      let name = "items_by_view_and_kind"
    end)
    (Items)
    (Items.Key_view_kind)

module ItemsById =
  Indexed_db.Make_index
    (struct
      let name = "items_by_id"
    end)
    (Items)
    (Items.Key_id)

module ItemsByTypeAndName =
  Indexed_db.Make_index
    (struct
      let name = "items_by_type_and_name"
    end)
    (Items)
    (Items.Key_type_name)

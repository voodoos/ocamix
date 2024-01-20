open! Std
open Brrer
open Brr_io

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
    let path = "id"
  end

  let name = "items_by_date_added"
  let to_jv t = t_to_jv yojson_of_t t
  let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
  let get_key t = t.id
end

module Items = struct
  open Data_source.Jellyfin.Api

  type sorts = { date_added : int; views : string list } [@@deriving yojson]
  type t = { sorts : sorts; item : Item.t } [@@deriving yojson]

  module Key = struct
    type t = string

    let to_jv k = Jv.of_string k
    let of_jv j = Jv.to_string j
    let path = "item.Id"
  end

  module Key_date_added = struct
    type t = int

    let to_jv k = Jv.of_int k
    let of_jv j = Jv.to_int j
    let path = "sorts.date_added"
  end

  module Key_view_name = struct
    type t = { views : string list; sort_name : string }

    let to_jv k = Jv.(of_jv_array [| Jv.of_list of_string k.views |])

    let of_jv j =
      match Jv.(to_jv_array j) with
      | [| views; sort_name |] ->
          {
            views = Jv.(to_list to_string views);
            sort_name = Jv.to_string sort_name;
          }
      | _ -> assert false

    let path = "sorts.views,item.SortName"
  end

  let name = "items"
  let to_jv t = t_to_jv yojson_of_t t
  let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
  let get_key t = t.item.Item.id
end

module Virtual_folder = struct
  open Data_source.Jellyfin_api

  (* todo: multiserver: we should add a server_id key *)
  type t = Virtual_folders.virtual_folder [@@deriving yojson]

  module Key = struct
    type t = string

    let to_jv k = Jv.of_string k
    let of_jv j = Jv.to_string j
    let path = "ItemId"
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

module ItemsByViewAndName =
  Indexed_db.Make_index
    (struct
      let name = "items_by_view_and_name"
    end)
    (Items)
    (Items.Key_view_name)

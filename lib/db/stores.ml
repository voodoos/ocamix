open! Std
open Brrer
open Brr_io.Indexed_db
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

  let name = "items_by_date_added"

  let to_jv t =
    let obj =
      Jv.obj
        [|
          ("id", Jv.of_int t.id);
          ("item", Jv.of_option ~none:Jv.null Jv.of_string t.item);
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

  module Key_date_added = struct
    type t = int

    let to_jv k = Jv.of_int k
    let of_jv j = Jv.to_int j
  end

  module Key_id = struct
    type t = string

    let to_jv k = Jv.of_string k
    let of_jv j = Jv.to_string j
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
  end

  let name = "items"
  let to_jv t = t_to_jv yojson_of_t t
  let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
end

module Virtual_folder = struct
  open Data_source.Jellyfin_api

  (* todo: multiserver: we should add a server_id key *)
  type t = Virtual_folders.virtual_folder [@@deriving yojson]

  let name = "virtual_folders"
  let to_jv t = t_to_jv yojson_of_t t
  let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
end

module Orderred_items_store =
  Make_object_store
    (Orderred_items)
    (struct
      type t = int

      let to_jv k = Jv.of_int k
      let of_jv j = Jv.to_int j
    end)

module Items_store_key = struct
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
end

module Items_store = Make_object_store (Items) (Items_store_key)

module Virtual_folder_store =
  Make_object_store
    (Virtual_folder)
    (struct
      type t = string

      let to_jv k = Jv.of_string k
      let of_jv j = Jv.to_string j
    end)

module ItemsByDateAdded =
  Make_index (Orderred_items_store) (Items.Key_date_added)

module ItemsByViewAndKind = Make_index (Items_store) (Items.Key_view_kind)
module ItemsById = Make_index (Items_store) (Items.Key_id)
module ItemsByTypeAndName = Make_index (Items_store) (Items.Key_type_name)

(* WIP New generic database schema:
   - Instead of storing Jellyfin specific items we process them to fit a more
     conventionnal schema. *)
open Generic_schema

module Collection = struct
  type t = Collection.t [@@deriving yojson]

  let name = "collections"
  let to_jv t = t_to_jv yojson_of_t t
  let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
end

module Collections_store = Make_object_store (Collection) (Auto_increment)

module Collections_by_id =
  Make_index
    (Collections_store)
    (struct
      include Generic_schema.Id

      let to_jv t = t_to_jv yojson_of_t t
      let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
    end)

module Genres = struct
  type t = Genre.t [@@deriving yojson]

  let name = "genres"
  let to_jv t = t_to_jv yojson_of_t t
  let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
end

module Genres_store = Make_object_store (Genres) (Auto_increment)

module Genres_by_canonical_name =
  Make_index
    (Genres_store)
    (struct
      type t = string

      let of_jv = Jv.to_string
      let to_jv = Jv.of_string
    end)

module Album = struct
  type t = Album.t [@@deriving yojson]

  let name = "albums"
  let to_jv t = t_to_jv yojson_of_t t
  let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
end

module Albums_store =
  Make_object_store
    (Album)
    (struct
      include Generic_schema.Album.Key

      let to_jv { name; genres } =
        let name = Jv.of_string name in
        let genres = Jv.of_list Jv.of_int genres in
        Jv.of_jv_array [| name; genres |]

      let of_jv j =
        match Jv.to_jv_array j with
        | [| name; genres |] ->
            let name = Jv.to_string name in
            let genres = Jv.to_list Jv.to_int genres in
            { name; genres }
        | _ -> assert false
    end)

module Albums_by_id =
  Make_index
    (Albums_store)
    (struct
      include Generic_schema.Id

      let to_jv t = t_to_jv yojson_of_t t
      let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
    end)

module Track = struct
  type t = Track.t [@@deriving yojson]

  let name = "tracks"
  let to_jv t = t_to_jv yojson_of_t t
  let of_jv j = jv_to_t t_of_yojson j |> Result.get_exn
end

module Tracks_store =
  Make_object_store
    (Track)
    (struct
      include Generic_schema.Track.Key

      let to_jv { name; genres; collections } =
        let name = Jv.of_string name in
        let genres = Jv.of_list Jv.of_int genres in
        let collections = Jv.of_list Jv.of_int collections in
        Jv.of_jv_array [| name; genres; collections |]

      let of_jv j =
        match Jv.to_jv_array j with
        | [| name; genres; collections |] ->
            let name = Jv.to_string name in
            let genres = Jv.to_list Jv.to_int genres in
            let collections = Jv.to_list Jv.to_int collections in
            { name; genres; collections }
        | _ -> assert false
    end)

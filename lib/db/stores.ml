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

module type Jsontable = sig
  type t [@@deriving jsont]
end

module Of_jsontable (M : Jsontable) = struct
  include M

  let to_jv t = Jsont_brr.encode_jv M.jsont t |> Result.get_exn
  let of_jv j = Jsont_brr.decode_jv M.jsont j |> Result.get_exn
end

module String_key = struct
  type t = string

  let to_jv k = Jv.of_string k
  let of_jv j = Jv.to_string j
end

module Int_key = struct
  type t = int

  let of_jv = Jv.to_int
  let to_jv = Jv.of_int
end

module Id_key = Of_jsontable (Generic_schema.Id)

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

(* WIP New generic database schema:
   - Instead of storing Jellyfin specific items we process them to fit a more
     conventionnal schema. *)
open Generic_schema

module Collection = struct
  include Of_jsontable (Collection)

  let name = "collections"
end

module Collections_store = Make_object_store (Collection) (Auto_increment)

module Collections_by_id =
  Make_index (Collections_store) (Of_jsontable (Generic_schema.Id))

let collections_by_id store =
  Collections_store.index (module Collections_by_id) ~name:"by-id" store

module Genres = struct
  include Of_jsontable (Generic_schema.Genre)

  let name = "genres"
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

module Artist = struct
  include Of_jsontable (Generic_schema.Artist)

  let name = "artists"
end

module Artists_store = Make_object_store (Artist) (Auto_increment)
module Artists_by_id = Make_index (Artists_store) (Id_key)
module Artists_by_mbid = Make_index (Artists_store) (String_key)

module Album = struct
  include Of_jsontable (Album)

  let name = "albums"
end

module Albums_store =
  Make_object_store
    (Album)
    (struct
      include Generic_schema.Album.Key

      let to_jv { id; name; genres; artists } =
        let id = match id with Jellyfin id -> Jv.of_string ("J " ^ id) in
        let name = Jv.of_string name in
        let genres = Jv.of_list Jv.of_int genres in
        let artists = Jv.of_list Jv.of_int artists in
        Jv.of_jv_array [| id; name; genres; artists |]

      let of_jv j =
        match Jv.to_jv_array j with
        | [| id; name; genres; artists |] ->
            let id =
              match String.split_on_char ~by:' ' @@ Jv.to_string id with
              | [ "J"; id ] -> Generic_schema.Id.Jellyfin id
              | _ -> assert false
            in
            let name = Jv.to_string name in
            let genres = Jv.to_list Jv.to_int genres in
            let artists = Jv.to_list Jv.to_int artists in
            { id; name; genres; artists }
        | _ -> assert false
    end)

module Albums_by_id = Make_index (Albums_store) (Id_key)
module Albums_by_idx = Make_index (Albums_store) (Int_key)

module Track = struct
  include Of_jsontable (Generic_schema.Track)

  let name = "tracks"
end

module Tracks_store =
  Make_object_store
    (Track)
    (struct
      include Generic_schema.Track.Key

      let to_jv { id; name; genres; collections; artists } =
        let id = match id with Jellyfin id -> Jv.of_string ("J " ^ id) in
        let name = Jv.of_string name in
        let genres = Jv.of_list Jv.of_int genres in
        let artists = Jv.of_list Jv.of_int artists in
        let collections = Jv.of_list Jv.of_int collections in
        Jv.of_jv_array [| id; name; genres; collections; artists |]

      let of_jv j =
        match Jv.to_jv_array j with
        | [| id; name; genres; collections; artists |] ->
            let id =
              match String.split_on_char ~by:' ' @@ Jv.to_string id with
              | [ "J"; id ] -> Generic_schema.Id.Jellyfin id
              | _ -> assert false
            in
            let name = Jv.to_string name in
            let genres = Jv.to_list Jv.to_int genres in
            let artists = Jv.to_list Jv.to_int artists in
            let collections = Jv.to_list Jv.to_int collections in
            { id; name; genres; collections; artists }
        | _ -> assert false
    end)

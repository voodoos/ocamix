open! Std
open Brrer
open Brr_io

module Orderred_items = struct
  type t = { id : int; item : string option } [@@deriving yojson]
  type key = int

  let name = "items_by_date_added"

  let to_jv t =
    yojson_of_t t |> Yojson.Safe.to_string |> Jstr.of_string |> Brr.Json.decode
    |> Result.get_exn

  let of_jv j =
    let json = Brr.Json.encode j in
    Ok (Jstr.to_string json |> Yojson.Safe.from_string |> t_of_yojson)

  let key_to_jv k = Jv.of_int k
  let key_of_jv j = Jv.to_int j
  let key_path = "id"
  let get_key t = t.id
end

module Orderred_items_store = Indexed_db.Make_object_store (Orderred_items)

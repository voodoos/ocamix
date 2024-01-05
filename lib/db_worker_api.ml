open! Std
module Api = Data_source.Jellyfin.Api

module View = struct
  type t = { uuid : Uuidm.t; item_count : int }
end

module Queries = struct
  type 'a query =
    | Get_all : unit -> Api.Item.t list query
    | Create_view : unit -> View.t query
    (* | Get : View.t * int -> Api.Item.t query *)
    | Get : int -> Api.Item.t query
end

include Worker_api.Make (Queries)

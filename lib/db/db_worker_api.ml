open! Std
module DS = Data_source.Jellyfin
module Api = DS.Api

type server = string * DS.connexion

module View = struct
  type t = { uuid : Uuidm.t; item_count : int }
end

module Queries = struct
  type 'a query =
    | Servers : server list -> unit query
    | Get_all : unit -> Api.Item.t list query
    | Create_view : unit -> View.t query
    (* | Get : View.t * int -> Api.Item.t query *)
    | Get : int -> Stores.Items.t query
end

include Worker_api.Make (Queries)

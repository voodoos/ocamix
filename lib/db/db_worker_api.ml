open! Std
module DS = Data_source.Jellyfin
module Api = DS.Api

type server = string * DS.connexion

module Queries = struct
  type 'a query =
    | Servers : server list -> unit query
    | Get_all : unit -> Api.Item.t list query
    | Create_view : View.req -> View.t query
    | Get : View.t * int -> Stores.Items.t query

  type 'a event = Servers_status_update : Sync.progress event
end

include Worker_api.Make (Queries)

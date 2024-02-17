open! Std
module DS = Data_source.Jellyfin
module Api = DS.Api

type server = string * DS.connexion

module Queries = struct
  type 'a query =
    | Add_servers : server list -> unit query
    | Get_all : unit -> Api.Item.t list query
    | Get_libraries : unit -> Stores.Items.t list query
    | Create_view : View.req -> View.t query
    | Get : View.t * int array -> Stores.Items.t option array query

  type 'a event = Servers_status_update : (string * Sync.report) event
end

include Worker_api.Make (Queries)

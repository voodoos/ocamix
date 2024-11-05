open! Std
module DS = Data_source.Jellyfin
module Api = DS.Api

type server = string * DS.connexion

module Queries = struct
  type 'a query =
    | Set_session_uuid : string -> unit query
    | Add_servers : server list -> unit query
    | Get_all : unit -> Api.Item.t list query
    | Get_libraries : unit -> (int * Stores.Collection.t) array query
    | Create_view : View.req -> View.t query
    | Get :
        View.t * View.Order.t * int array
        -> (Generic_schema.Track.Key.t * Generic_schema.Track.t) option array
           query

  type 'a event = Servers_status_update : (string * Sync.report) event
end

include Worker_api.Make (Queries)

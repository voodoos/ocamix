open! Std
module DS = Data_source.Jellyfin
module Api = DS.Api
open Generic_schema

type server = string * DS.connexion

module Queries = struct
  type 'a query =
    | Set_session_uuid : string -> unit query
    | Add_servers : server list -> unit query
    | Get_libraries : unit -> (int * Stores.Collection.t) array query
    | Create_view : View.req -> View.t query
    | Get_view_albums : View.t -> (int * Genre.t) Int.Map.t query
    | Get :
        View.t * View.Order.t * int array
        -> (Track.Key.t * Track.t) option array query

  type 'a event = Servers_status_update : (string * Sync.report) event
end

include Worker_api.Make (Queries)

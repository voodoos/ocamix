open! Std
module DS = Data_source.Jellyfin
module Api = DS.Api
open Generic_schema
module List = Stdlib.List

type server = string * DS.connexion [@@deriving jsont]

module Queries = struct
  type set_session_uuid = string [@@deriving jsont]
  type add_servers = server list [@@deriving jsont]
  type libraries = (int * Stores.Collection.t) array [@@deriving jsont]
  type view = View.t [@@deriving jsont]
  type create_view = View.req [@@deriving jsont]
  type get = view * View.Order.t * int array [@@deriving jsont]
  type genres = (int * Genre.t) Int.Map.t [@@deriving jsont]
  type artists = Artist.t info Int.Map.t [@@deriving jsont]
  type tracks = (Track.Key.t * Track.t) option array [@@deriving jsont]

  type ('a, 'b) query =
    | Set_session_uuid : (set_session_uuid, unit) query
    | Add_servers : (add_servers, unit) query
    | Get_libraries : (unit, libraries) query
    | Create_view : (create_view, view) query
    | Get_view_genres : (view, genres) query
    | Get_view_artists : (view, artists) query
    | Get : (get, tracks) query

  let jsont (type a b) (q : (a, b) query) : a Jsont.t * b Jsont.t =
    match q with
    | Set_session_uuid -> (set_session_uuid_jsont, Jsont.null ())
    | Add_servers -> (add_servers_jsont, Jsont.null ())
    | Get_libraries -> (Jsont.null (), libraries_jsont)
    | Create_view -> (create_view_jsont, view_jsont)
    | Get_view_genres -> (view_jsont, genres_jsont)
    | Get_view_artists -> (view_jsont, artists_jsont)
    | Get -> (get_jsont, tracks_jsont)

  type servers_status_update = string * Sync.report [@@deriving jsont]
  type 'a event = Servers_status_update : servers_status_update event

  let event_jsont (type a) (e : a event) : a Jsont.t =
    match e with Servers_status_update -> servers_status_update_jsont
end

include Worker_api.Make (Queries)

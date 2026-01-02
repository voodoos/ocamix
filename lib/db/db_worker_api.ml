open! Std
open Brr
open Worker_api
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
  type get = view * int array [@@deriving jsont]
  type genres = (int * Genre.t) Int.Map.t [@@deriving jsont]
  type artists = Artist.t info Int.Map.t [@@deriving jsont]

  type tracks = (Track.Key.t * Track.t * Album.t option) option array
  [@@deriving jsont]

  type ('a, 'b) query =
    | Set_session_uuid : (set_session_uuid, unit) query
    | Add_servers : (add_servers, unit) query
    | Get_libraries : (unit, libraries) query
    | Create_view : (create_view, view) query
    | Create_album_view :
        ( create_view,
          view
          * (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t )
        query
    | Get_view_genres : (view, genres) query
    | Get_view_artists : (view, artists) query
    | Get_tracks : (get, tracks) query

  let conv a = Conv a

  let view_keys_array_transfert =
    let open Result.Infix in
    let j_view = Jstr.v "v" in
    let j_keys = Jstr.v "k" in
    {
      encode =
        (fun (view, tarray) ->
          let+ view =
            Jsont_brr.encode_jv view_jsont view
            |> Result.map_err (fun e -> `Jv e)
          in
          Jv.obj'
            [|
              (j_view, view); (j_keys, Tarray.to_jv (Tarray.of_bigarray1 tarray));
            |]);
      decode =
        (fun jv ->
          let v = Jv.get' jv j_view in
          let k = Jv.get' jv j_keys in
          let+ view =
            Jsont_brr.decode_jv view_jsont v |> Result.map_err (fun e -> `Jv e)
          in
          (view, Tarray.of_jv k |> Tarray.to_bigarray1));
      transferables =
        [
          (fun jv ->
            Jv.get' jv j_keys |> Tarray.of_jv |> Tarray.buffer
            |> Tarray.Buffer.to_jv);
        ];
    }

  let null = Conv (Jsont.null ())

  let jsont (type a b) (q : (a, b) query) :
      a Jsont.t * b Worker_api.transfer_or_conv =
    match q with
    | Set_session_uuid -> (set_session_uuid_jsont, null)
    | Add_servers -> (add_servers_jsont, null)
    | Get_libraries -> (Jsont.null (), Conv libraries_jsont)
    | Create_view -> (create_view_jsont, Conv view_jsont)
    | Create_album_view ->
        (create_view_jsont, Transfer view_keys_array_transfert)
    | Get_view_genres -> (view_jsont, Conv genres_jsont)
    | Get_view_artists -> (view_jsont, Conv artists_jsont)
    | Get_tracks -> (get_jsont, Conv tracks_jsont)

  type servers_status_update = string * Sync.report [@@deriving jsont]
  type 'a event = Servers_status_update : servers_status_update event

  let event_jsont (type a) (e : a event) : a Jsont.t =
    match e with Servers_status_update -> servers_status_update_jsont
end

include Worker_api.Make (Queries)

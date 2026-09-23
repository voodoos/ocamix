open Std
open Brr
module Api = Jellyfin_api

type credentials = { base_url : string; username : string; password : string }

type connexion = {
  base_url : string;
  auth_response : Api.Authenticate_by_name.response;
}
[@@deriving jsont]

let get_token t = t.auth_response.Api.Authenticate_by_name.access_token

let connect credentials =
  let module Auth = Api.Authenticate_by_name in
  let open Fut.Result_syntax in
  let { base_url; username; password } = credentials in
  let auth = { Api.Authenticate_by_name.username; pw = password } in
  let+ auth_response = Api.request ~base_url (module Auth) auth () in
  { base_url; auth_response }

let query t =
  let token = get_token t in
  let base_url = t.base_url in
  Jellyfin_api.request ~base_url ~token

type play_method =
  | Direct_play  (** Direct streaming *)
  | Direct_stream  (** Remux in a compatible container *)
  | Transcode

let pp_play_method fmt = function
  | Direct_play -> Format.pp_print_string fmt "direct play"
  | Direct_stream -> Format.pp_print_string fmt "direct stream"
  | Transcode -> Format.pp_print_string fmt "transcode"

type stream = {
  url : string;
  play_method : play_method;
  container : string option;
}

(** Direct (streaming) url for a media source the browser can decode as is. See
    [getStreamUrls] in jellyfin-web's [playbackmanager.js]. *)
let direct_stream_url t ~item_id (source : Api.Playback_info.media_source_info)
    =
  let container =
    Option.map_or ~default:"" String.lowercase_ascii source.container
  in
  let uri =
    Api.uri_of_endpoint ~base_url:t.base_url
      [ "Audio"; item_id; "stream." ^ container ]
  in
  let maybe_param name v = Option.map (Pair.make name) v in
  let params =
    List.filter_map ~f:Fun.id
      [
        Some ("Static", "true");
        Some ("ApiKey", get_token t);
        maybe_param "mediaSourceId" source.id;
        maybe_param "deviceId" !Api.session_uuid;
        maybe_param "Tag" source.etag;
        maybe_param "LiveStreamId" source.live_stream_id;
      ]
    |> List.map ~f:(fun (k, v) -> (Jstr.v k, Jstr.v v))
    |> Uri.Params.of_assoc
  in
  Uri.with_query_params uri params |> Uri.to_jstr |> Jstr.to_string

(** Transcoding urls are given relative to the server's root. *)
let transcoding_url t path =
  String.rdrop_while ~f:(Char.equal '/') t.base_url ^ path

let stream_of_source t ~item_id (source : Api.Playback_info.media_source_info) =
  let container = source.container in
  match (source.supports_direct_play, source.supports_direct_stream) with
  | Some true, _ ->
      Some
        {
          url = direct_stream_url t ~item_id source;
          play_method = Direct_play;
          container;
        }
  | _, Some true ->
      Some
        {
          url = direct_stream_url t ~item_id source;
          play_method = Direct_stream;
          container;
        }
  | _ ->
      Option.map
        (fun path ->
          {
            url = transcoding_url t path;
            play_method = Transcode;
            container = source.transcoding_container;
          })
        source.transcoding_url

(** Ask the server how this item should be played, given what we can decode, and
    build the resulting url. *)
let audio_stream t ~device_profile ~item_id =
  let open Fut.Result_syntax in
  let params =
    Api.Playback_info.params ~user_id:t.auth_response.user.id ~device_profile
      ~enable_direct_play:true ~enable_direct_stream:true
      ~enable_transcoding:true ~allow_audio_stream_copy:true ()
  in
  let+ response =
    query t (module Api.Playback_info) params { Api.Playback_info.item_id }
  in
  match response.error_code with
  | Some code ->
      Console.error
        [
          "Jellyfin refused to play this item:";
          code;
          Option.value ~default:"" response.error_message;
        ];
      None
  | None -> (
      match response.media_sources with
      | [] -> None
      | source :: _ -> stream_of_source t ~item_id source)

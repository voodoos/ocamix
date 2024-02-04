open! Import
open Brr
open Brr_lwd

type playstate = { playlist : Db.View.t; current_index : int }
type t = Elwd.t Lwd.t

let playstate = Lwd.var None

type now_playing = { item_id : string; url : string }

let now_playing = Lwd.var None

(* see https://github.com/jellyfin/jellyfin/blob/4786901bb796c3e912f13b686571fde8d16f49c5/tests/Jellyfin.Model.Tests/Test%20Data/DeviceProfile-Firefox.json *)

(** Playback issue with alac files (and probably all codecs non-natively
      supported by the browser)
    - Some clients (sonixd) give up, mpv based players work.
    - The official client manage to play the file but gives a blob to the audio
      player instead of a hls url as it does for other files.
    - Jellyfin should use the device id and associated capabilities to
      automatically transcode, shouldn't it ?

*)
let audio_url (server : DS.connexion) item_id =
  Printf.sprintf
    "%s/Audio/%s/universal?api_key=%s&audioCodec=aac&container=opus,mp3,aac,m4a,m4b,flac,wav,ogg&transcodingContainer=ts&transcodingProtocol=hls"
    server.base_url item_id server.auth_response.access_token

module Playback_controller (P : sig
  val fetch :
    Db.View.t -> int -> (Db.Stores.Items.t, Db.Worker_api.error) Fut.result
end) =
struct
  let set_play_url { playlist; current_index } =
    let open Fut.Result_syntax in
    let+ item =
      let+ { Db.Stores.Items.item = { server_id; id; name; _ }; _ } =
        P.fetch playlist current_index
      in
      let servers = Lwd_seq.to_list (Lwd.peek Servers.var) in
      let server : Servers.server = List.assq server_id servers in
      let url = audio_url server.connexion id in
      let () = Console.log [ "Now playing:"; name; Jv.of_string url ] in
      { item_id = id; url }
    in
    Lwd.set now_playing (Some item)

  let reset_playlist playlist =
    let state = { playlist; current_index = 0 } in
    ignore @@ set_play_url state;
    Lwd.set playstate (Some state)

  let make () =
    let src =
      Lwd.get now_playing
      |> Lwd.map ~f:(function
           | None -> At.src (Jstr.v "")
           | Some { url; _ } -> At.src (Jstr.v url))
    in
    let next _ =
      Lwd.peek playstate
      |> Option.iter (fun (ps : playstate) ->
             let new_state = { ps with current_index = ps.current_index + 1 } in
             ignore @@ set_play_url new_state;
             Lwd.set playstate (Some new_state))
    in

    let on_ended = Elwd.handler Ev.ended next in
    let on_error =
      Elwd.handler Ev.error (fun ev ->
          Ev.stop_immediate_propagation ev;
          Ev.prevent_default ev;
          Console.log
            [
              "A playback error happened. This is probably due to a codec \
               unsupported by the browser.";
              ev;
            ];
          next ev)
    in
    let ev = [ `P on_ended; `P on_error ] in
    let audio =
      Elwd.audio
        ~at:
          [
            `P (At.v (Jstr.v "controls") (Jstr.v "true"));
            `P (At.v (Jstr.v "autoplay") (Jstr.v "true"));
            `P (At.v (Jstr.v "preload") (Jstr.v "auto"));
            `R src;
          ]
        ~ev []
    in
    let btn_next =
      Brr_lwd_ui.Button.v ~ev:[ `P (Elwd.handler Ev.click next) ] (`P "NEXT")
    in
    let at = [ `P (At.class' (Jstr.v "player-wrapper")) ] in
    Elwd.div ~at [ `R audio; `R btn_next ]
end

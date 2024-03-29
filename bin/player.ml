open! Import
open Brr
open Brr_lwd

type playstate = {
  playlist : Db.View.t option Lwd.var;
  current_index : int Lwd.var;
}

type t = Elwd.t Lwd.t

let playstate = { playlist = Lwd.var None; current_index = Lwd.var 0 }

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
    Db.View.t ->
    int array ->
    (Db.Stores.Items.t option array, Db.Worker_api.error) Fut.result
end) =
struct
  let set_play_url playlist current_index =
    match playlist with
    | None -> Fut.ok ()
    | Some playlist ->
        let open Fut.Result_syntax in
        let+ item =
          let+ result = P.fetch playlist [| current_index |] in
          match result with
          | [| Some { Db.Stores.Items.item = { server_id; id; name; _ }; _ } |]
            ->
              let servers = Lwd_seq.to_list (Lwd.peek Servers.var) in
              let server : Servers.server = List.assq server_id servers in
              let url = audio_url server.connexion id in
              let () = Console.log [ "Now playing:"; name; Jv.of_string url ] in
              let () =
                let open Brr_io.Media.Session in
                let session = of_navigator G.navigator in
                let img_src =
                  Printf.sprintf "%s/Items/%s/Images/Primary?width=500"
                    server.connexion.base_url id
                in
                let title = name in
                let album = "" in
                let artist = "" in
                let artwork =
                  [
                    {
                      Media_metadata.src = img_src;
                      sizes = "500x500";
                      type' = "";
                    };
                  ]
                in
                set_metadata session { title; artist; album; artwork }
              in
              { item_id = id; url }
          | _ -> raise Not_found
        in
        Lwd.set now_playing (Some item)

  let reset_playlist playlist =
    ignore @@ set_play_url (Some playlist) 0;
    Lwd.set playstate.playlist (Some playlist);
    Lwd.set playstate.current_index 0

  let make () =
    let src =
      Lwd.get now_playing
      |> Lwd.map ~f:(function
           | None -> At.src (Jstr.v "")
           | Some { url; _ } -> At.src (Jstr.v url))
    in
    let next () =
      let playlist = Lwd.peek playstate.playlist in
      let current_index = Lwd.peek playstate.current_index in
      let next_index = current_index + 1 in
      ignore @@ set_play_url playlist next_index;
      Lwd.set playstate.current_index next_index
    in
    let () =
      let open Brr_io.Media.Session in
      let session = of_navigator G.navigator in
      set_action_handler session Action.next_track next
    in
    let next _ = next () in
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

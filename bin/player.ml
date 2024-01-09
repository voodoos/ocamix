open! Import
open Brr
open Brr_lwd

type playstate = { playlist : Db.View.t; current_index : int }
type t = Elwd.t Lwd.t

let playstate = Lwd.var None
let source_url = Lwd.var None

let audio_url (server : DS.connexion) item_id =
  Printf.sprintf
    "%s/Audio/%s/universal?api_key=%s&MaxStreamingBitrate=178723404&Container=opus,webm|opus,mp3,aac,m4a|aac,m4b|aac,flac,webma,webm|webma,wav,ogg&TranscodingContainer=ts&TranscodingProtocol=hls&AudioCodec=aac"
    server.base_url item_id server.auth_response.access_token

module Playback_controller (P : sig
  val fetch :
    Db.View.t -> int -> (Db.Stores.Items.t, Db.Worker_api.error) Fut.result

  val servers : ((string * DS.connexion) list, Db.Worker_api.error) Fut.result
end) =
struct
  let set_play_url { playlist; current_index } =
    let open Fut.Result_syntax in
    let+ url =
      let* { Db.Stores.Items.item = { server_id; id; name; _ }; _ } =
        P.fetch playlist current_index
      in
      let+ servers = P.servers in
      let server : DS.connexion = List.assq server_id servers in
      let () = Console.log [ "Now playing:"; name ] in
      audio_url server id
    in
    let () = Console.log [ "next"; url ] in
    Lwd.set source_url (Some url)

  let reset_playlist playlist =
    let state = { playlist; current_index = 0 } in
    let () = Console.log [ "new playlist"; playlist ] in
    ignore @@ set_play_url state;
    Lwd.set playstate (Some state)

  let make () =
    let src =
      Lwd.get source_url
      |> Lwd.map ~f:(fun url -> At.src (Jstr.v (Option.value ~default:"" url)))
    in
    Elwd.audio
      ~at:
        [
          `P (At.v (Jstr.v "controls") (Jstr.v "true"));
          `P (At.v (Jstr.v "autoplay") (Jstr.v "true"));
          `P (At.v (Jstr.v "preload") (Jstr.v "auto"));
          `R src;
        ]
      []
end

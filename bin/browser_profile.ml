open Import
open Brr
module DP = Data_source.Device_profile

(** The [DeviceProfile] we advertise to Jellyfin. Rather than hardcoding a list
    of containers we ask the browser what it can decode, so that a file the
    current browser handles natively is streamed as is and everything else gets
    transcoded by the server. See jellyfin-web's [browserDeviceProfile.js]. *)

let can_play =
  let audio = lazy (El.audio []) in
  fun mime ->
    let audio = El.to_jv (Lazy.force audio) in
    let answer = Jv.call audio "canPlayType" [| Jv.of_string mime |] in
    (* "", "maybe" or "probably" *)
    not (Jstr.is_empty (Jv.to_jstr answer))

(* Some containers cannot be probed by their own name. *)
let mime_type = function
  | "opus" -> {|audio/ogg; codecs="opus"|}
  | "webma" -> "audio/webm"
  (* [audio/alac] is not a thing, but Safari does answer for alac in mp4. *)
  | "alac" -> {|audio/mp4; codecs="alac"|}
  | format -> "audio/" ^ format

let supported formats = List.filter ~f:(fun f -> can_play (mime_type f)) formats

let direct_play_profiles () =
  supported
    [ "opus"; "mp3"; "aac"; "flac"; "alac"; "webma"; "wav"; "ogg"; "oga" ]
  |> List.concat_map ~f:(fun format ->
      DP.direct_play format
      ::
      (match format with
      | "opus" | "webma" -> [ DP.direct_play ~audio_codec:format "webm" ]
      (* aac and alac also show up in the m4a and m4b containers *)
      | "aac" | "alac" ->
          [
            DP.direct_play ~audio_codec:format "m4a";
            DP.direct_play ~audio_codec:format "m4b";
          ]
      | _ -> []))

(* Progressive http only: browsers' [audio] element cannot play hls outside of
   Safari. The server picks the first profile it can satisfy, so the order
   matters. *)
let transcoding_profiles () =
  supported [ "aac"; "mp3"; "opus"; "wav" ]
  |> List.map ~f:(fun format ->
      DP.transcoding ~protocol:DP.Protocol.Http ~audio_codec:format
        ~max_audio_channels:"2" format)

let t =
  lazy
    {
      DP.empty with
      name = Some "Ocamix";
      music_streaming_transcoding_bitrate = Some 384_000;
      direct_play_profiles = direct_play_profiles ();
      transcoding_profiles = transcoding_profiles ();
    }

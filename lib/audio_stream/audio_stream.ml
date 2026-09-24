open Brr
open! Brr_webaudio.Audio
module Media_el = Brr_io.Media.El
module Source = Node.Media_element_source
open Fut.Result_syntax

let fade_duration_ms = 5000.
let fade_duration_s = fade_duration_ms /. 1000.

type track_nodes = { source : Source.t; gain : Node.Gain.t }

type t = {
  audio_context : Context.t;
  mutable current : track_nodes option;
  mutable next : track_nodes option;
  queue : Source.t Queue.t;  (** the playlist *)
}

let context t = Context.as_base t.audio_context
let current_time t = context t |> Context.Base.current_time

let init () =
  let audio_context = Context.create () in
  { audio_context; current = None; next = None; queue = Queue.create () }

let prepare_next t =
  Console.log [ "Prepare next." ];
  let next =
    Queue.take_opt t.queue
    |> Option.map @@ fun source ->
       Console.log
         [ "Next song is:"; Source.media_element source |> Media_el.src ];
       let context = context t in
       let gain = Node.Gain.create ~opts:(Node.Gain.opts ~gain:0. ()) context in
       let gain_node = Node.Gain.as_node gain in
       let destination =
         Context.Base.destination context |> Node.Destination.as_node
       in
       Node.connect_node (Source.as_node source) ~dst:gain_node;
       Node.connect_node gain_node ~dst:destination;
       { source; gain }
  in
  t.next <- next

let stop_and_disconnect_current t =
  Option.iter
    (fun { source; gain } ->
      Source.media_element source |> Media_el.pause;
      Node.disconnect (Node.Gain.as_node gain))
    t.current

let cancel_and_hold t param =
  let time = current_time t in
  let value = Param.value param in
  Param.cancel_scheduled_values param ~time;
  Param.set_value_at_time param ~value ~time

let ramp_param t ~fade ~duration_s param value =
  let now = current_time t in
  cancel_and_hold t param;
  if (not fade) || Float.equal duration_s 0. then Param.set_value param value
  else
    let end_time = now +. duration_s in
    Param.linear_ramp_to_value_at_time param ~value ~end_time

let rec start_playing t ~fade_in { source; gain } =
  let media = Source.media_element source in
  Console.log [ "Start playing "; Media_el.src media ];
  let+ () = Media_el.play media in
  let track_duration_s = Media_el.duration_s media in
  let fade_duration_s = Float.min fade_duration_s (track_duration_s /. 2.) in
  let pgain = Node.Gain.gain gain in
  let () =
    ramp_param t ~fade:fade_in ~duration_s:fade_duration_s pgain 1.;
    Console.log
      [ "Track duration:"; track_duration_s; "Fade in:"; fade_duration_s ]
  in
  let _ =
    let media = Source.media_element source in
    let threshold = track_duration_s -. fade_duration_s in
    Console.log [ "Will crossfade in"; threshold ];
    let listener = ref None in
    listener :=
      Some
        (Media_el.to_el media |> El.as_target
        |> Ev.listen Ev.timeupdate (fun _ev ->
            let media_current_time_s = Media_el.current_time_s media in
            if media_current_time_s > threshold then begin
              Option.iter
                (fun next ->
                  (* TODO The correct thing to do would be to know the duration
                     of the next track early to not start the fade if it is
                     shorter than it. *)
                  Ev.unlisten (Option.get !listener);
                  let duration_s = track_duration_s -. media_current_time_s in
                  Console.log [ "Start crossfade: fade out:"; duration_s ];
                  ramp_param t ~fade:true ~duration_s pgain 0.;
                  ignore
                  @@ let+ () = start_playing t ~fade_in:true next in
                     t.current <- t.next;
                     prepare_next t)
                t.next
            end))
  in
  let _ =
    let opts = Ev.listen_opts ~once:true () in
    Media_el.to_el media |> El.as_target
    |> Ev.listen ~opts Ev.ended (fun _ ->
        Source.media_element source |> Media_el.pause;
        Node.disconnect (Node.Gain.as_node gain);
        Console.log [ "Ended" ])
  in
  ()

let force_next t =
  Console.log [ "Force next" ];
  (* Stop the current playing track if any *)
  (* TODO option to crossfade *)
  stop_and_disconnect_current t;
  (* Promote the next source *)
  t.current <- t.next;
  (* Start it *)
  let+ () =
    Option.fold t.current ~none:(Fut.ok ())
      ~some:(start_playing t ~fade_in:false)
  in
  prepare_next t

let queue_song t url =
  Console.log [ "Queue song:"; url ];
  let audio_el =
    El.audio
      ~at:
        [
          At.v (Jstr.v "controls") (Jstr.v "false");
          At.v (Jstr.v "autoplay") (Jstr.v "false");
          At.v (Jstr.v "preload") (Jstr.v "auto");
          At.src (Jstr.v url);
        ]
      []
  in
  let context = Context.as_base t.audio_context in
  let source =
    let el = Media_el.of_el audio_el in
    Media_el.pause el;
    Console.log [ "Next song is:"; el |> Media_el.src ];
    let opts = Source.opts ~el () in
    Source.create context ~opts
  in
  Queue.add source t.queue

let resume t =
  match (t.current, t.next) with
  | Some { source; _ }, _ -> Source.media_element source |> Media_el.play
  | None, Some _ -> force_next t
  | None, None ->
      prepare_next t;
      force_next t

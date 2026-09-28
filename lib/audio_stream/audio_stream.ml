open Brr
open! Brr_webaudio.Audio
module Media_el = Brr_io.Media.El
module Source = Node.Media_element_source
open Fut.Result_syntax

let fade_duration_ms = 5000.
let fade_duration_s = fade_duration_ms /. 1000.

type track_nodes = { source : Source.t; gain : Node.Gain.t }
type state = [ `Playing | `Paused ]
type playback_infos = { fade_out_start_time : float; track_duration_s : float }
type progress = { current : float; total : float }

type t = {
  audio_context : Context.t;
  mutable current : track_nodes option;
  mutable next : track_nodes option;
  queue : Source.t Queue.t;  (** the playlist *)
  mutable transition_in_progress : bool;
  mutable transition_id : int;
  on_state_change : state -> unit;
  on_progress : progress -> unit;
  on_track_change : playback_infos -> unit;
}

let context t = Context.as_base t.audio_context
let current_time t = context t |> Context.Base.current_time

let current_media_element t =
  Option.map (fun { source; _ } -> Source.media_element source) t.current

let init ?on_progress ?on_state_change ?on_track_change () =
  let on_progress = Option.value ~default:(Fun.const ()) on_progress in
  let on_track_change = Option.value ~default:(Fun.const ()) on_track_change in
  let on_state_change = Option.value ~default:(Fun.const ()) on_state_change in
  let audio_context = Context.create () in
  {
    audio_context;
    current = None;
    next = None;
    queue = Queue.create ();
    transition_in_progress = false;
    transition_id = 0;
    on_progress;
    on_track_change;
    on_state_change;
  }

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

let reset t =
  t.transition_id <- t.transition_id + 1;
  t.transition_in_progress <- false;
  stop_and_disconnect_current t;
  Option.iter
    (fun { source; gain } ->
      Source.media_element source |> Media_el.pause;
      Node.disconnect (Node.Gain.as_node gain))
    t.next;
  Queue.clear t.queue;
  t.current <- None;
  t.next <- None;
  t.on_state_change `Paused

let start_transition t =
  t.transition_id <- t.transition_id + 1;
  t.transition_in_progress <- true;
  t.transition_id

let finish_transition t id f =
  if id = t.transition_id then begin
    t.transition_in_progress <- false;
    f ()
  end

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
  t.on_state_change `Playing;
  let track_duration_s = Media_el.duration_s media in
  let fade_duration_s = Float.min fade_duration_s (track_duration_s /. 2.) in
  let pgain = Node.Gain.gain gain in
  let () =
    ramp_param t ~fade:fade_in ~duration_s:fade_duration_s pgain 1.;
    Console.log
      [ "Track duration:"; track_duration_s; "Fade in:"; fade_duration_s ]
  in
  let threshold = track_duration_s -. fade_duration_s in
  Console.log [ "Will crossfade in"; threshold ];
  t.on_track_change { fade_out_start_time = threshold; track_duration_s };
  let _ =
    let listener = ref None in
    listener :=
      Some
        (Media_el.to_el media |> El.as_target
        |> Ev.listen Ev.timeupdate (fun _ev ->
            let media_current_time_s = Media_el.current_time_s media in
            t.on_progress
              { current = media_current_time_s; total = track_duration_s };
            if media_current_time_s > threshold then begin
              Option.iter
                (fun next ->
                  (* TODO The correct thing to do would be to know the duration
                     of the next track early to not start the fade if it is
                     shorter than it. *)
                  if not t.transition_in_progress then begin
                    Ev.unlisten (Option.get !listener);
                    let id = start_transition t in
                    let duration_s = track_duration_s -. media_current_time_s in
                    Console.log [ "Start crossfade: fade out:"; duration_s ];
                    ramp_param t ~fade:true ~duration_s pgain 0.;
                    let playing = start_playing t ~fade_in:true next in
                    Fut.await playing (function
                      | Ok () ->
                          finish_transition t id (fun () ->
                              t.current <- Some next;
                              prepare_next t)
                      | Error error ->
                          finish_transition t id (fun () ->
                              Console.error
                                [
                                  "Unable to start crossfade track:";
                                  Jv.Error.message error;
                                ];
                              ramp_param t ~fade:false ~duration_s:0. pgain 1.))
                  end)
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
  if t.transition_in_progress then Fut.ok ()
  else
    match t.next with
    | None -> Fut.ok ()
    | Some next ->
        (* Do not accept another skip until [play] settles. Otherwise quick
           clicks promote the same [next] node several times before
           [prepare_next] can replace it. *)
        let id = start_transition t in
        stop_and_disconnect_current t;
        t.current <- Some next;
        let playing = start_playing t ~fade_in:false next in
        Fut.await playing (function
          | Ok () -> finish_transition t id (fun () -> prepare_next t)
          | Error error ->
              finish_transition t id (fun () ->
                  Console.error
                    [ "Unable to start next track:"; Jv.Error.message error ];
                  t.current <- None));
        playing

let queue_song t url =
  Console.log [ "Queue song:"; url ];
  let audio_el =
    El.audio
      ~at:
        [
          At.v (Jstr.v "controls") (Jstr.v "false");
          At.v (Jstr.v "autoplay") (Jstr.v "false");
          At.v (Jstr.v "preload") (Jstr.v "true");
          (* This must be set before [src]. A media element loaded without a
             CORS mode may play normally, but Web Audio is required to silence
             it when it is passed to [createMediaElementSource]. The Jellyfin
             URLs carry their API key in the query string, so no cookies are
             needed. *)
          At.v (Jstr.v "crossorigin") (Jstr.v "anonymous");
        ]
      []
  in
  El.set_at (Jstr.v "src") (Some (Jstr.v url)) audio_el;
  let context = Context.as_base t.audio_context in
  let source =
    let el = Media_el.of_el audio_el in
    Media_el.pause el;
    Console.log [ "Next song is:"; el |> Media_el.src ];
    let opts = Source.opts ~el () in
    Source.create context ~opts
  in
  Queue.add source t.queue;
  if Option.is_some t.current && Option.is_none t.next then prepare_next t

let pause t =
  Option.iter
    (fun { source; _ } ->
      let media = Source.media_element source in
      Media_el.pause media;
      t.on_state_change `Paused)
    t.current

let resume t =
  match (t.current, t.next) with
  | Some { source; _ }, _ ->
      let+ () = Source.media_element source |> Media_el.play in
      t.on_state_change `Playing
  | None, Some _ -> force_next t
  | None, None ->
      prepare_next t;
      force_next t

let seek t time_s =
  Option.iter
    (fun { source; _ } ->
      let media = Source.media_element source in
      Media_el.set_current_time_s media time_s)
    t.current

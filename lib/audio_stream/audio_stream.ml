open Brr
open! Brr_webaudio.Audio
module Media_el = Brr_io.Media.El
module Source = Node.Media_element_source
open Fut.Result_syntax

let fade_duration_ms = 5000.
let fade_duration_s = fade_duration_ms /. 1000.
let track_id = ref 0

type status = [ `Playing | `Paused ]

type playback_infos = {
  id : int;  (** Identifies the track, as returned by [queue_song] *)
  fade_out_start_time : float;
  track_duration_s : float;
}

type progress = { current : float; total : float }

type queue = (int * Source.t) Queue.t
(** Queued tracks, paired with the id [queue_song] handed back for them. *)

let clear_queue queue =
  Queue.iter
    (fun (_id, source) -> Source.media_element source |> Media_el.pause)
    queue;
  Queue.clear queue

type track_infos = {
  id : int;
  source : Source.t;
  gain : Node.Gain.t;
  mutable fade_in : bool;  (** Ramp the gain up when the track is started *)
  mutable started : bool;  (** [start_playing] ran: the listeners are attached *)
}

type cruise_state = { current : track_infos; next : track_infos; queue : queue }

type crossfading_state = {
  cruise : cruise_state;
  mutable fade_end_time : float;
      (** Context time at which the fade completes *)
  mutable fade_remaining_s : float;
      (** What was left of the fade when it was paused *)
}

type playlist_state =
  | Stopped of { was_playing : bool }
      (** Nothing playing or ready to be played. If [was_playing] is true the
          next prepared track will be autoplayed. s *)
  | Last of { current : track_infos }
      (** Current track is playing, nothing in the queue *)
  | Cruise of cruise_state
      (** Current track is playing, next + queue is not empty *)
  | Crossfading of crossfading_state  (** Transition between two tracks *)

type t = {
  audio_context : Context.t;
  mutable state : playlist_state;
  on_status_change : status -> unit;
  on_progress : progress -> unit;
  on_track_change : playback_infos -> unit;
}

let context t = Context.as_base t.audio_context
let current_time t = context t |> Context.Base.current_time

let current_media_element t =
  match t.state with
  | Stopped _ -> None
  | Last { current = { source; _ } } | Cruise { current = { source; _ }; _ } ->
      Some (Source.media_element source)
  | Crossfading { cruise = { next = { source; _ }; _ }; _ } ->
      (* As soon as a crossfade started we consider the next track to be the
         main one. *)
      Some (Source.media_element source)

let current_track_id t =
  match t.state with
  | Stopped _ -> None
  | Last { current } | Cruise { current; _ } -> Some current.id
  | Crossfading { cruise = { next; _ }; _ } -> Some next.id

let init ?on_progress ?on_status_change ?on_track_change () =
  let on_progress = Option.value ~default:(Fun.const ()) on_progress in
  let on_track_change = Option.value ~default:(Fun.const ()) on_track_change in
  let on_status_change =
    Option.value ~default:(Fun.const ()) on_status_change
  in
  let audio_context = Context.create () in
  {
    audio_context;
    state = Stopped { was_playing = false };
    on_progress;
    on_track_change;
    on_status_change;
  }

let prepare_source t ~id source =
  Console.log [ "Preparing:"; Source.media_element source |> Media_el.src ];
  let context = context t in
  let gain = Node.Gain.create ~opts:(Node.Gain.opts ~gain:0. ()) context in
  let gain_node = Node.Gain.as_node gain in
  let destination =
    Context.Base.destination context |> Node.Destination.as_node
  in
  Node.connect_node (Source.as_node source) ~dst:gain_node;
  Node.connect_node gain_node ~dst:destination;
  { id; source; gain; fade_in = false; started = false }

let disconnect_track { source; gain; _ } =
  Source.media_element source |> Media_el.pause;
  Node.disconnect (Node.Gain.as_node gain)

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

(** Make [next] the current track and pull a new [next] out of the queue. *)
let promote t { current; next; queue } =
  disconnect_track current;
  let current = next in
  match Queue.take_opt queue with
  | Some (id, source) ->
      t.state <- Cruise { current; next = prepare_source t ~id source; queue }
  | None -> t.state <- Last { current }

let rec start_playing t ({ id; source; gain; fade_in; _ } as track) =
  let media = Source.media_element source in
  Console.log [ "Start playing "; Media_el.src media ];
  let+ () = Media_el.play media in
  track.started <- true;
  t.on_status_change `Playing;
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
  t.on_track_change { id; fade_out_start_time = threshold; track_duration_s };
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
              Ev.unlisten (Option.get !listener);
              maybe_crossfade t id
            end))
  in
  let _ =
    let opts = Ev.listen_opts ~once:true () in
    Media_el.to_el media |> El.as_target
    |> Ev.listen ~opts Ev.ended (fun _ ->
        Console.log [ "Ended" ];
        on_ended t id)
  in
  ()

(** The current track is about to end: start fading into [next]. *)
and maybe_crossfade t request_id =
  match t.state with
  | Stopped _ | Last _ | Crossfading _ -> ()
  | Cruise ({ current; next; _ } as cruise) ->
      if request_id = current.id then begin
        let media = Source.media_element current.source in
        let pgain = Node.Gain.gain current.gain in
        let track_duration_s = Media_el.duration_s media in
        let media_current_time_s = Media_el.current_time_s media in
        let duration_s =
          Float.max 0. (track_duration_s -. media_current_time_s)
        in
        Console.log [ "Start crossfade: fade out:"; duration_s ];
        t.state <-
          Crossfading
            {
              cruise;
              fade_end_time = current_time t +. duration_s;
              fade_remaining_s = duration_s;
            };
        ramp_param t ~fade:true ~duration_s pgain 0.;
        next.fade_in <- true;
        let playing = start_playing t next in
        Fut.await playing (function
          | Ok () -> ()
          | Error error -> (
              (* A [force_next] during the fade pauses this element while [play]
                 is still pending, rejecting it with [AbortError]. *)
              match t.state with
              | Crossfading { cruise; _ } when cruise.current.id = request_id ->
                  Console.error
                    [
                      "Unable to start crossfade track:"; Jv.Error.message error;
                    ];
                  ramp_param t ~fade:false ~duration_s:0. pgain 1.;
                  t.state <- Cruise cruise
              | _ -> ()))
      end

and on_ended t request_id =
  match t.state with
  | Stopped _ -> ()
  | Last { current } when current.id = request_id ->
      disconnect_track current;
      t.state <- Stopped { was_playing = true };
      t.on_status_change `Paused
  | Crossfading { cruise; _ } when cruise.current.id = request_id ->
      (* Normal end of a crossfade: the incoming track takes over. *)
      ramp_param t ~fade:false ~duration_s:0.
        (Node.Gain.gain cruise.next.gain)
        1.;
      promote t cruise
  | Cruise ({ current; next; _ } as cruise) when current.id = request_id ->
      (* The track ended without crossfading. *)
      next.fade_in <- false;
      ignore (start_playing t next);
      promote t cruise
  | Last _ | Cruise _ | Crossfading _ -> ()

let rec force_next t =
  Console.log [ "Force next" ];
  match t.state with
  | Stopped _ | Last _ -> Fut.ok ()
  | Cruise cruise -> try_next t cruise
  | Crossfading { cruise; _ } ->
      (* [next] is already playing: snap it to full gain and drop the outgoing
         track instead of waiting for the fade to complete. *)
      (* ramp_param t ~fade:false ~duration_s:0. (Node.Gain.gain next.gain) 1.; *)
      promote t cruise;
      force_next t

and try_next t ({ next; _ } as cruise) =
  next.fade_in <- false;
  let playing = start_playing t next in
  (* Promote synchronously so that quick clicks cannot promote the same
         node twice while [play] is still settling. *)
  promote t cruise;
  Fut.await playing (function
    | Ok () -> ()
    | Error error ->
        (* A further [force_next] pauses this element while [play] is still
               pending, which rejects it with [AbortError]. Only a track that is
               still current actually failed to load. *)
        if Option.equal Int.equal (current_track_id t) (Some next.id) then begin
          Console.error
            [ "Unable to start next track:"; Jv.Error.message error ];
          (* Skip the broken track. *)
          ignore (force_next t)
        end);
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
             it when it is passed to [createMediaElementSource]. *)
          At.v (Jstr.v "crossorigin") (Jstr.v "anonymous");
        ]
      []
  in
  let el = Media_el.of_el audio_el in
  Media_el.set_src el (Jstr.v url);
  let context = context t in
  let source =
    Media_el.pause el;
    Console.log [ "Next song is:"; el |> Media_el.src ];
    let opts = Source.opts ~el () in
    Source.create context ~opts
  in
  let id = !track_id in
  incr track_id;
  (match t.state with
  | Stopped { was_playing } ->
      let current = prepare_source t ~id source in
      t.state <- Last { current };
      (* The queue ran dry while playing: pick playback back up. *)
      if was_playing then ignore (start_playing t current)
  | Last { current } ->
      let next = prepare_source t ~id source in
      t.state <- Cruise { current; next; queue = Queue.create () }
  | Cruise { queue; _ } | Crossfading { cruise = { queue; _ }; _ } ->
      Queue.add (id, source) queue);
  id

let reset t =
  (match t.state with
  | Stopped _ -> ()
  | Last { current } -> disconnect_track current
  | Cruise { current; next; queue }
  | Crossfading { cruise = { current; next; queue }; _ } ->
      disconnect_track current;
      disconnect_track next;
      clear_queue queue);
  (* this is a deliberate stop *)
  t.state <- Stopped { was_playing = false };
  t.on_status_change `Paused

let pause t =
  match t.state with
  | Stopped _ -> ()
  | Last { current } | Cruise { current; _ } ->
      Source.media_element current.source |> Media_el.pause;
      t.on_status_change `Paused
  | Crossfading ({ cruise = { current; next; _ }; fade_end_time; _ } as cf) ->
      (* Freeze them and remember position. TODO it might be simpler to pause
         the audio context if it also pauses the sources. *)
      cf.fade_remaining_s <- Float.max 0. (fade_end_time -. current_time t);
      cancel_and_hold t (Node.Gain.gain current.gain);
      cancel_and_hold t (Node.Gain.gain next.gain);
      Source.media_element current.source |> Media_el.pause;
      Source.media_element next.source |> Media_el.pause;
      t.on_status_change `Paused

(** Play [track], starting it if it has never been started. *)
let resume_track t track =
  if track.started then
    let+ () = Source.media_element track.source |> Media_el.play in
    t.on_status_change `Playing
  else start_playing t track

let resume t =
  match t.state with
  | Stopped _ -> Fut.ok ()
  | Last { current } | Cruise { current; _ } -> resume_track t current
  | Crossfading ({ cruise = { current; next; _ }; fade_remaining_s; _ } as cf)
    ->
      let duration_s = fade_remaining_s in
      ramp_param t ~fade:true ~duration_s (Node.Gain.gain current.gain) 0.;
      ramp_param t ~fade:true ~duration_s (Node.Gain.gain next.gain) 1.;
      cf.fade_end_time <- current_time t +. duration_s;
      let current_resume = resume_track t current in
      let next_resume = resume_track t next in
      Fut.bind current_resume (fun _ -> next_resume)

let seek t time_s =
  match t.state with
  | Stopped _ -> ()
  | Last { current = { source; _ } }
  | Cruise { current = { source; _ }; _ }
  | Crossfading { cruise = { next = { source; _ }; _ }; _ } ->
      (* TODO we could seek both and readjust the gain but we need a parameter
         to provide user intent (is the user seeking current or next or in the
         crossfade) *)
      let media = Source.media_element source in
      Media_el.set_current_time_s media time_s

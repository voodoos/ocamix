open Brrer
open Brr
open Brr_lwd

type state = {
  stream : Audio_stream.t;
  status : Audio_stream.state Lwd.var;
  playback_infos : Audio_stream.playback_infos Lwd.var;
  progress : Audio_stream.progress Lwd.var;
}

let tick_width = 2
let tick_spacing = 5
let timeline_width = Lwd.var 0

let observer =
  let callback entries _ =
    let w =
      List.hd entries |> Resize_observer.Entry.content_rect
      |> Dom_rect_read_only.width
    in
    Console.log [ w ];
    Lwd.set timeline_width w
  in
  Resize_observer.create ~callback

let timeline { stream; progress; playback_infos; _ } =
  let ticks =
    Lwd.map (Lwd.get timeline_width) ~f:(fun size ->
        let ticks_number = size / (tick_width + tick_spacing) in
        Console.log [ "Ticks number:"; ticks_number ];
        (* It's a bit janky rigth now *)
        let current_tick =
          Lwd.map (Lwd.get progress) ~f:(fun { current; total } ->
              Float.of_int (ticks_number - 1) *. (current /. total)
              |> Float.round |> Float.to_int)
        in
        let fade_out_start_tick =
          Lwd.map (Lwd.get playback_infos)
            ~f:(fun { fade_out_start_time; track_duration_s } ->
              Console.log [ "FOST "; fade_out_start_time; track_duration_s ];
              Float.of_int (ticks_number - 1)
              *. (fade_out_start_time /. track_duration_s)
              |> Float.round |> Float.to_int)
        in
        List.init ticks_number (fun i ->
            let c_active =
              Lwd.map current_tick ~f:(fun t ->
                  if t = i then At.class' (Jstr.v "active") else At.void)
            in
            let c_fade_out =
              Lwd.map fade_out_start_tick ~f:(fun t ->
                  Console.log [ "FOST "; t ];
                  if t = i then At.class' (Jstr.v "fade-out") else At.void)
            in
            let classes =
              Lwd.return @@ Lwd_seq.of_list [ c_active; c_fade_out ]
            in
            let seek =
              Lwd.map (Lwd.get playback_infos)
                ~f:(fun { track_duration_s; _ } ->
                  Elwd.handler Ev.click @@ fun _ ->
                  let time_s =
                    Float.of_int i
                    *. (track_duration_s /. Float.of_int ticks_number)
                  in
                  Console.log [ "Seek"; time_s ];
                  Audio_stream.seek stream time_s)
            in
            Elwd.div
              ~at:[ `S (Lwd_seq.lift classes) ]
              ~ev:[ `R seek ]
              [ `P (El.div []) ])
        |> Lwd_seq.of_list)
  in
  Elwd.div
    ~on_create:(Resize_observer.observe observer)
    ~at:[ `P (At.class' (Jstr.v "ap-timeline")) ]
    [ `S (Lwd_seq.lift ticks) ]

let time_s_to_string ?(force_hours = false) s =
  let s = s |> Float.floor |> Float.to_int in
  let m = s / 60 in
  let s = s mod 60 in
  let h = m / 60 in
  let m = m mod 60 in
  if force_hours || h > 0 then Printf.sprintf "%02i:%02i:%02i" h m s
  else Printf.sprintf "%02i:%02i" m s

let timer { playback_infos; progress; _ } =
  (* TODO same smoothing issue as the moving tick *)
  let total =
    Lwd.map (Lwd.get playback_infos) ~f:(fun { track_duration_s; _ } ->
        El.txt' (time_s_to_string track_duration_s))
  in
  let current =
    Lwd.map (Lwd.get progress) ~f:(fun { current; total } ->
        let force_hours = total > 3600. in
        El.txt' (time_s_to_string ~force_hours current))
  in
  Elwd.div
    ~at:[ `P (At.class' (Jstr.v "ap-timer")) ]
    [ `R (Elwd.span [ `R current; `P (El.txt' " / "); `R total ]) ]

let play_pause { stream; status; _ } ev =
  Lwd.map (Lwd.get status) ~f:(fun state ->
      Elwd.handler ev @@ fun _ ->
      match state with
      | `Playing -> Audio_stream.pause stream
      | `Paused -> ignore @@ Audio_stream.resume stream)

let play_btn state =
  let v =
    Lwd.map (Lwd.get state.status) ~f:(function
      | `Playing -> El.txt' "❘❘"
      | `Paused -> El.txt' "▷")
  in
  Elwd.div
    ~at:[ `P (At.class' (Jstr.v "ap-play-btn")) ]
    ~ev:[ `R (play_pause state Ev.click) ]
    [ `R (Elwd.button [ `R (Elwd.span [ `R v ]) ]) ]

let controls state =
  Elwd.div
    ~at:[ `P (At.class' (Jstr.v "ap-controls")) ]
    (* TODO play/pause on space ~ev:[ `R (play_pause Ev.keyup) ] *)
    [ `R (play_btn state); `R (timeline state); `R (timer state) ]

let make_player () =
  let status = Lwd.var `Paused in
  let playback_infos =
    Lwd.var { Audio_stream.fade_out_start_time = 0.; track_duration_s = 0. }
  in
  let progress = Lwd.var { Audio_stream.current = 0.; total = 0. } in
  let stream =
    Audio_stream.init ~on_progress:(Lwd.set progress)
      ~on_track_change:(Lwd.set playback_infos)
      ~on_state_change:(Lwd.set status) ()
  in
  let state = { stream; status; playback_infos; progress } in
  (controls state, state.stream)

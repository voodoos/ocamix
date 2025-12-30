open Brr

let now_ms () = Performance.now_ms G.performance |> Float.to_int

(** [limit ~interval_ms:50 f] wraps [f] in a function that can be executed at
    most once every 50ms. The last call will always happens even if it as made
    during the debouncing_interval. *)
let limit ?(interval_ms = 50) f =
  (* We use [last_update] to have regular  updates and the [timeout] to ensure
     that the last event is always taken into account even it it happens during
     the debouncing interval. *)
  let last_update = ref 0 in
  let trailing_edge = ref false in
  let timeout = ref (-1) in
  let reset_trailing () =
    trailing_edge := true;
    timeout := -1
  in
  let run_trailing_edge () =
    if !trailing_edge then begin
      last_update := now_ms ();
      f ()
    end;
    reset_trailing ()
  in
  fun () ->
    let now = now_ms () in
    let time_elapsed_since_last_trigger = now - !last_update in
    if time_elapsed_since_last_trigger >= interval_ms then begin
      last_update := now;
      if !timeout >= 0 then G.stop_timer !timeout;
      reset_trailing ();
      f ()
    end
    else begin
      trailing_edge := true;
      if !timeout < 0 then
        let ms =
          (* Remaining time before the trailing edge *)
          interval_ms - time_elapsed_since_last_trigger
        in
        timeout := G.set_timeout ~ms run_trailing_edge
    end

(* TODO : unify with [limit] if possible *)
type throttler = ?delay:bool -> (unit -> unit) -> unit

(** [throttle ~delay_ms] creates a throttler. Successive uses of that throttler
    with be limited to one every [delay_ms] milliseconds. The last call to the
    throttler will always be honoured.

    When sending work to a throttler there are two modes:
    - Delayed: the function is always executed with a delay, even the first
      call. New queued calls cancel the previous ones if they are done before
      [delay_ms]. This is useful for debouncing text input for example, were we
      only care about the last input.
    - Immediate: the function is executed immediately once every [delay_ms] this
      is useful for filtering notification messages for example, were we want
      regular updates but at most one per delay. *)
let throttle ~delay_ms : throttler =
  (* We use [last_update] to have regular debounced updates and the
     [timeout] to ensure that the last event is always taken into
     account even it it happens during the debouncing interval. *)
  let last_update = ref 0. in
  let timer = ref (-1) in
  fun ?(delay = false) f ->
    let now = Performance.now_ms G.performance in
    if !timer >= 0 then G.stop_timer !timer;
    if now -. !last_update > float_of_int delay_ms then (
      last_update := now;
      if delay then timer := G.set_timeout ~ms:delay_ms f else f ())
    else timer := G.set_timeout ~ms:delay_ms f

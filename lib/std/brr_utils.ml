open Brr

let with_timing name f =
  let open Brr.Performance in
  let before = now_ms G.performance in
  let result = f () in
  Console.log [ name; " took"; now_ms G.performance -. before; "ms" ];
  result

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

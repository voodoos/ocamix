open Import
open Brr

type 'a one_maybe_reactive = [ `P of 'a | `R of 'a Lwd.t ]
type 'a maybe_reactive = [ 'a one_maybe_reactive | `S of 'a Lwd_seq.t Lwd.t ]

let is_pure_element = function
  | `P _ -> true
  | `R x -> Option.is_some (Lwd.is_pure x)
  | `S x -> Option.is_some (Lwd.is_pure x)

let pure t = `P t
let reactive t = `R t
let sequence t = `S t

let tap ?(initial_trigger = false) ~f t =
  let root = Lwd.observe t in
  Lwd.set_on_invalidate root (fun _ ->
      (* See https://github.com/let-def/lwd/issues/52 *)
      Window.queue_micro_task G.window (fun () -> f (Lwd.quick_sample root)));
  let first_sample = Lwd.quick_sample root in
  if initial_trigger then f first_sample

let collect_into_var t =
  let root = Lwd.observe t in
  let first_sample = Lwd.quick_sample root in
  let v = Lwd.var first_sample in
  Lwd.set_on_invalidate root (fun _ ->
      Window.queue_micro_task G.window (fun () ->
          Lwd.set v (Lwd.quick_sample root)));
  v

let set_if_different ?(equal = Equal.poly) var v =
  if equal v @@ Lwd.peek var then () else Lwd.set var v

let map3 ~f a b c =
  Lwd.map2 a b ~f:(fun a b -> (a, b)) |> Lwd.map2 c ~f:(fun c (a, b) -> f a b c)

let triple a b c = map3 a b c ~f:(fun a b c -> (a, b, c))

module Forward_ref : sig
  type 'a t

  exception Not_set
  exception Already_set

  val make : unit -> 'a t
  val set_exn : 'a t -> 'a -> unit
  val get_exn : 'a t -> 'a
end = struct
  type 'a t = 'a option ref

  exception Not_set
  exception Already_set

  let make () = ref None

  let set_exn t v =
    match !t with None -> t := Some v | Some _ -> raise Already_set

  let get_exn t = match !t with None -> raise Not_set | Some v -> v
end

let var_of_fut ~init fut =
  let v = Lwd.var init in
  Fut.await fut (Lwd.set v);
  v

let var_of_fut_opt fut =
  let v = Lwd.var None in
  Fut.await fut (fun r -> Lwd.set v @@ Option.some r);
  v

let wait_and_set v fut = Fut.await fut (Lwd.set v)

let measure_execution_time name f () =
  let before = Brr.Performance.now_ms G.performance in
  let result = f () in
  Console.debug
    [
      Printf.sprintf "%s took %fms" name
        (Brr.Performance.now_ms G.performance -. before);
    ];
  result

module Sort = struct
  type 'data compare =
    | Compare : {
        proj : 'data -> 'value;
        compare : 'value -> 'value -> int;
      }
        -> 'data compare

  let int ?(proj = Fun.id) () = Compare { proj; compare = Int.compare }
  let string ?(proj = Fun.id) () = Compare { proj; compare = String.compare }
  let compare (Compare sort) d1 d2 = sort.compare (sort.proj d1) (sort.proj d2)

  let reverse (Compare { proj; compare }) =
    let compare a b = compare b a in
    Compare { proj; compare }

  let lwd_seq compare t =
    let compare (v1, i1) (v2, i2) =
      let c = compare v1 v2 in
      if c = 0 then Int.compare i1 i2 else c
    in
    let i = ref 0 in
    t
    |> Lwd_seq.map (fun v ->
        incr i;
        (v, !i))
    |> Lwd_seq.sort_uniq compare |> Lwd_seq.map fst
end

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

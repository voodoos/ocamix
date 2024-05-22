(* The implementation is inspired by Sieve:
   - https://cachemon.github.io/SIEVE-website/
   - https://junchengyang.com/publication/nsdi24-SIEVE.pdf

   Sieve uses a doubly-linked list and a "had pointer".
   We use a FIFO and replace the hand by another FIFO.

   The idea was suggested by @art-w *)

(* A [RA_queue] pairs a [Queue.t] with a [Hashtbl.t] to enable efficient random
   access to elements of the queue. *)
module RA_queue = struct
  type ('key, 'v) t = { queue : 'key Queue.t; elts : ('key, 'v) Hashtbl.t }

  let create () = { queue = Queue.create (); elts = Hashtbl.create 64 }

  let clear t =
    Queue.clear t.queue;
    Hashtbl.clear t.elts

  let add t k x =
    Queue.add k t.queue;
    Hashtbl.add t.elts k x

  let take_opt t =
    match Queue.take_opt t.queue with
    | None -> None
    | Some k ->
        let x = Hashtbl.find t.elts k in
        Hashtbl.remove t.elts k;
        Some x

  let find t k = Hashtbl.find_opt t.elts k
end

type 'a elt = { elt : 'a; visited : bool ref }

type 'a t = {
  q1 : ('a, 'a elt) RA_queue.t;
  q2 : ('a, 'a elt) RA_queue.t;
  size : int;
  hand : 'a elt option;
  on_insert : 'a -> unit;
  on_evict : 'a -> unit;
}

let empty ~size ~on_insert ~on_evict =
  let q1 = RA_queue.create () in
  let q2 = RA_queue.create () in
  { q1; q2; size; hand = None; on_insert; on_evict }

let clear t =
  RA_queue.clear t.q1;
  RA_queue.clear t.q2

(** [evict_one t] first tries to evict the last element of [t.q2].
    If that last element has been visited, it is moved to the head of [t.q1].
    Loop until an element is evicted or [t.q2] is empty.
    If [t.q2] is empty we perform the same process by inversing the roles of
    [t.q2] and [t.q1].

    /!\ This function will loop if both queues are empty. *)
let rec evict_one t = evict_q2 t

and evict_q2 t =
  match RA_queue.take_opt t.q2 with
  | Some { elt; visited } when !visited ->
      RA_queue.add t.q1 elt { elt; visited = ref false };
      evict_q2 t
  | Some { elt; _ } -> t.on_evict elt
  | None -> evict_q1 t

and evict_q1 t =
  match RA_queue.take_opt t.q1 with
  | Some { elt; visited } when !visited ->
      RA_queue.add t.q2 elt { elt; visited = ref false };
      evict_q1 t
  | Some { elt; _ } -> t.on_evict elt
  | None -> evict_q2 t

let rec evict t =
  let size = Queue.length t.q1.queue + Queue.length t.q2.queue in
  if size > 0 && size > t.size then (
    evict_one t;
    evict t)

let insert t x =
  match RA_queue.find t.q1 x with
  | Some { elt; visited } ->
      (* If the elt is already in q1 we mark it as visited *)
      visited := true;
      false
  | None -> (
      match RA_queue.find t.q2 x with
      (* If the elt is already in q2 we mark it as visited *)
      | Some { elt; visited } ->
          visited := true;
          false
      | None ->
          (* If the elt was not yet in the cache we add it to q1 *)
          RA_queue.add t.q1 x { elt = x; visited = ref false };
          t.on_insert x;
          evict t;
          true)

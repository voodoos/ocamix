(* The implementation is inspired by Sieve:
   - https://cachemon.github.io/SIEVE-website/
   - https://junchengyang.com/publication/nsdi24-SIEVE.pdf

   Sieve uses a doubly-linked list and a "had pointer".
   We use a FIFO and replace the hand by another FIFO.

   The idea was suggested by @art-w *)

(* A [RA_queue] pairs a [Queue.t] with a [Hashtbl.t] to enable efficient random
   access to elements of the queue. *)
module RA_queue (Key : Map.OrderedType) = struct
  module Queue = CCFQueue
  module Map = Map.Make (Key)

  type 'v t = { queue : Key.t Queue.t; elts : 'v Map.t }

  let size t = Queue.size t.queue
  let create () = { queue = Queue.empty; elts = Map.empty }
  let _empty = { queue = Queue.empty; elts = Map.empty }
  let add t k x = { queue = Queue.cons k t.queue; elts = Map.add k x t.elts }

  let take_opt t =
    match Queue.take_back t.queue with
    | None -> (t, None)
    | Some (queue, k) ->
        let x = Map.find k t.elts in
        ({ queue; elts = Map.remove k t.elts }, Some (k, x))

  let find t k = Map.find_opt k t.elts
end

module type S = sig
  type key
  (** The type of the cache keys. *)

  type +!'a t
  (** The type of caches from type [key] to type ['a]. *)

  val create : size:int -> 'a t
  (** Creates an empty cache. *)

  val insert :
    'a t ->
    ?on_insert:('a -> unit) ->
    ?on_evict:('a -> unit) ->
    key ->
    'a ->
    'a t * bool
end

module Make (Key : Map.OrderedType) : S with type key = Key.t = struct
  module RA_queue = RA_queue (Key)

  type key = Key.t
  type 'a elt = { elt : 'a; visited : bool ref }
  type 'a t = { q1 : 'a elt RA_queue.t; q2 : 'a elt RA_queue.t; size : int }

  let create ~size =
    let q1 = RA_queue.create () in
    let q2 = RA_queue.create () in
    { q1; q2; size }

  (** [evict_one t] first tries to evict the last element of [t.q2]. If that
      last element has been visited, it is moved to the head of [t.q1]. Loop
      until an element is evicted or [t.q2] is empty. If [t.q2] is empty we
      perform the same process by inversing the roles of [t.q2] and [t.q1].

      /!\ This function will loop if both queues are empty. *)
  let rec evict_one ~on_evict t = evict_q2 ~on_evict t

  and evict_q2 ~on_evict t =
    match RA_queue.take_opt t.q2 with
    | q2, Some (k, { elt; visited }) when !visited ->
        let q1 = RA_queue.add t.q1 k { elt; visited = ref false } in
        evict_q2 ~on_evict { t with q1; q2 }
    | q2, Some (_k, { elt; _ }) ->
        on_evict elt;
        { t with q2 }
    | _, None -> evict_q1 ~on_evict t

  and evict_q1 ~on_evict t =
    match RA_queue.take_opt t.q1 with
    | q1, Some (k, { elt; visited }) when !visited ->
        let q2 = RA_queue.add t.q2 k { elt; visited = ref false } in
        evict_q1 ~on_evict { t with q1; q2 }
    | q1, Some (_k, { elt; _ }) ->
        on_evict elt;
        { t with q1 }
    | _, None -> evict_q2 ~on_evict t

  let rec evict ~on_evict t =
    let size = RA_queue.size t.q1 + RA_queue.size t.q2 in
    if size > 0 && size > t.size then evict ~on_evict (evict_one ~on_evict t)
    else t

  let insert t ?(on_insert = ignore) ?(on_evict = ignore) k x =
    match RA_queue.find t.q1 k with
    | Some { elt = _; visited } ->
        (* If the elt is already in q1 we mark it as visited *)
        visited := true;
        (t, false)
    | None -> (
        match RA_queue.find t.q2 k with
        (* If the elt is already in q2 we mark it as visited *)
        | Some { elt = _; visited } ->
            visited := true;
            (t, false)
        | None ->
            (* If the elt was not yet in the cache we add it to q1 *)
            let q1 = RA_queue.add t.q1 k { elt = x; visited = ref false } in
            on_insert x;
            (evict ~on_evict { t with q1 }, true))
end

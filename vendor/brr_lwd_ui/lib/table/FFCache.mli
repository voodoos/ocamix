(** A simple bounded cache with an eviction mechanism using 2 FIFOs. Inspired by
    SIEVE. Unproven. Untested. *)

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

module Make (Key : Map.OrderedType) : S with type key = Key.t

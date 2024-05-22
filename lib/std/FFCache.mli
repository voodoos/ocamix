(** A simple bounded cache with an eviction mechanism using 2 FIFOs.
    Inspired by SIEVE. Unproven. Untested. *)

type 'a t

val empty : size:int -> on_insert:('a -> unit) -> on_evict:('a -> unit) -> 'a t
val clear : 'a t -> unit
val insert : 'a t -> 'a -> bool

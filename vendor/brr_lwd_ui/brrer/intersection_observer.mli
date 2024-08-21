open Brr

type t

val of_jv : Jv.t -> t

module Entry : sig
  type t

  val of_jv : Jv.t -> t
  val is_intersecting : t -> bool
  val target : t -> El.t
end

val create : callback:(Entry.t list -> t -> unit) -> ?root:El.t -> unit -> t
(* TODO: [create] add missing options
   https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver/IntersectionObserver*)

val observe : t -> El.t -> unit

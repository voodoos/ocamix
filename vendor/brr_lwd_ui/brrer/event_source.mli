(** Bindings for the server-sent event API

    See https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events *)

include Jv.CONV

val as_target : t -> Brr.Ev.target
val create : url:Jstr.t -> unit -> t

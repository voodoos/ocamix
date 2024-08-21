(** Bindings to DOMRectReadOnly

    https://developer.mozilla.org/en-US/docs/Web/API/DOMRectReadOnly *)

type t

val of_jv : Jv.t -> t
val x : t -> int
val y : t -> int
val width : t -> int
val height : t -> int
val top : t -> int
val right : t -> int
val bottom : t -> int
val left : t -> int

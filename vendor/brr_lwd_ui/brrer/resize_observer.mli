(** Bindings to the ResizeObserver API
    @see https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver
*)

type t

val of_jv : Jv.t -> t

module Entry : sig
  type t

  val of_jv : Jv.t -> t

  type box_size = { block_size : int; inline_size : int }

  val box_size_of_jv : t -> box_size
  val border_box_size : t -> box_size array
  val content_box_size : t -> box_size array
  val content_rect : t -> Dom_rect_read_only.t
  val device_pixel_content_box_size : t -> box_size array
  val target : t -> Brr.El.t
end

type box = Content_box | Border_box | Device_pixel_content_box

val disconnect : t -> unit
val observe : t -> ?box:box -> Brr.El.t -> unit
val unobserve : t -> Brr.El.t -> unit
val create : callback:(Entry.t list -> t -> unit) -> t

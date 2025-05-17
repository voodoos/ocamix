open Brr
open Brr_lwd

type status = Ready | Loading | Error | Ok
type preloader

val pp_status : Format.formatter -> status -> unit
val preloader : ?placeholder:string -> string option Fut.t Lwd.t -> preloader
val status : preloader -> status Lwd.t

val loading_at : ?name:string -> preloader -> At.t Lwd.t
(** Reacts to a preloader with a class attribute with name "loading" if loading
    is in progress, [At.void] if not. [name] can be used to choose a different
    class name. *)

val object_url : preloader -> string option Lwd.t
(** Provides direct access to the image's object url for use as background for
    example. This is intended for "immediate" use and can be revoked by the
    preloader at any time. *)

val render_img :
  ?revoke_on_load:bool -> at:At.t Elwd.col -> preloader -> El.t Lwd.t

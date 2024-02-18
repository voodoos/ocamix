open Brr

type t

val of_jv : Jv.t -> t
val of_navigator : Navigator.t -> t

module Media_metadata : sig
  type img = { src : string; sizes : string; type' : string }

  type t = {
    title : string;
    artist : string;
    album : string;
    artwork : img list;
  }
end

val metadata : t -> Media_metadata.t option
val set_metadata : t -> Media_metadata.t -> unit

module Action : sig
  type t

  val next_track : t
end

(* TODO: the callback actully has an argument:
   https://developer.mozilla.org/en-US/docs/Web/API/MediaSession/setActionHandler#callback *)
val set_action_handler : t -> Action.t -> (unit -> unit) -> unit

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
  val previous_track : t
end

(* TODO: the callback actully has an argument:
   https://developer.mozilla.org/en-US/docs/Web/API/MediaSession/setActionHandler#callback *)

val set_action_handler : t -> Action.t -> (unit -> unit) -> unit
(** Sets a handler for a media session action. These actions let a web app
    receive notifications when the user engages a device's built-in physical or
    onscreen media controls, such as play, stop, or seek buttons.

    https://developer.mozilla.org/en-US/docs/Web/API/MediaSession/setActionHandler#callback *)

val set_position_state :
  ?duration:float -> ?playback_rate:float -> ?position:float -> t -> unit

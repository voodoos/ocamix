(** This module's API is still a work in progress and subject to changes. *)

open Brr

(** The target of a controlled scroll: can be a position in pixels or an DOM
  element. *)
type target = Pos of int | El of El.t

val make :
  ?at:[ `P of At.t | `R of At.t Lwd.t | `S of At.t Lwd_seq.t Lwd.t ] list ->
  scroll_target:target option Lwd.t ->
  El.t Lwd.t ->
  El.t Lwd.t
(** [make ?at ~scroll_target elt] will wrap [elt] in a div containing a floating
  button. When the button is pressed the [elt] scroll position will react to
  changes of [scroll_target]. Control is given back to the user as soon a manual
  scrolling is initiated.

  TODO:
   - More configuration options
   - Customizable button list
   - Control an element that is a child of [elt]
   - Default behavior for "scroll to top" *)

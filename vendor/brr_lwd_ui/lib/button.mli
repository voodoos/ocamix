open! Import
open Brr
open Brr_lwd

val v :
  ?d:El.document ->
  ?at:[ `P of At.t | `R of At.t Lwd.t | `S of At.t Lwd_seq.t Lwd.t ] list ->
  ?ev:Elwd.handler Elwd.col ->
  string Utils.maybe_reactive ->
  El.t Lwd.t

type 'a update = None | Next | Set of 'a
type 'state handler_with_state

val handler :
  ?opts:Ev.listen_opts ->
  'a Ev.type' ->
  ('b -> 'a Ev.t -> 'b update) ->
  'b handler_with_state

module type State = sig
  type t

  val default : t
  val next : t -> t
end

val with_state :
  ?base:Attrs.t ->
  (module State with type t = 't) ->
  ?state:'t ->
  ?d:El.document ->
  ?at:('t -> Attrs.t) ->
  ?ev:'t handler_with_state Elwd.col ->
  ('t -> El.t Elwd.col) ->
  El.t Lwd.t * (unit -> 't Lwd.t) * ('t -> unit)

type two_state = On | Off

module Two_state : State with type t = two_state

val two_state :
  ?base:Attrs.t ->
  ?state:Two_state.t ->
  ?d:El.document ->
  ?at:(Two_state.t -> Attrs.t) ->
  ?ev:Two_state.t handler_with_state Elwd.col ->
  (Two_state.t -> El.t Elwd.col) ->
  El.t Lwd.t * (unit -> Two_state.t Lwd.t) * (Two_state.t -> unit)

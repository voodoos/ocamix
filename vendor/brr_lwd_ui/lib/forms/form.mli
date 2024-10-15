open Brr_lwd
module Field = Field

type ('t, 'a) form_setter = 't -> 'a Field.validation -> 't
type 'res form_field

module type Form = sig
  type t

  val default : t
  val fields : t form_field Lwd.t Lwd_seq.t Lwd.t
end

val field : 'a Field.t Lwd.t -> ('b, 'a) form_setter -> 'b form_field Lwd.t
(** Attach a new field to a form with a function that sets the form's state with
    values from that field. *)

val create :
  ?d:Brr.El.document ->
  ?at:Brr.At.t Elwd.col ->
  ?ev:Elwd.handler Elwd.col ->
  (module Form with type t = 't) ->
  ('t -> unit) ->
  Brr.El.t Lwd.t

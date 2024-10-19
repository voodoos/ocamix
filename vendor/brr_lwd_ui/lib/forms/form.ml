open Import
open Brrer.Brr
open Brr_lwd
module Field = Field

type ('t, 'a) form_setter = 't -> 'a Field.validation -> 't

type 'res form_field =
  | F : 'a Field.t * ('form, 'a) form_setter -> 'form form_field

module type Form = sig
  type t

  val default : t
  val fields : t form_field Lwd.t Lwd_seq.t Lwd.t
end

let field field setter = Lwd.map field ~f:(fun field -> F (field, setter))

let create ?d ?at ?ev (type t) (module Form : Form with type t = t) on_submit :
    Elwd.t Lwd.t =
  let fields =
    Lwd_seq.lift Form.fields
    |> Lwd_seq.fold_monoid
         (fun (F (field, mapper)) ->
           ( Lwd_seq.element field.elt,
             let value () = Lwd.peek field.value |> field.validate in
             fun t -> mapper t @@ value () ))
         ( (Lwd_seq.empty, Fun.id),
           fun (elts, f) (elts', f') ->
             (Lwd_seq.concat elts elts', fun t -> f' (f t)) )
  in
  let handler =
    Lwd.map fields ~f:(fun (_, value) ->
        let on_submit e =
          on_submit @@ value Form.default;
          Ev.prevent_default e
        in
        Elwd.handler Ev.submit on_submit)
  in
  let on_submit = `R handler in
  let elts = Lwd.map fields ~f:(fun (elts, _) -> elts) in
  let ev = Option.map_or ~default:[ on_submit ] (List.cons on_submit) ev in
  Elwd.form ?d ?at ~ev [ `S (Lwd_seq.lift elts) ]

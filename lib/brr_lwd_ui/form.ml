open Std
open Brrer.Brr
open Brr_lwd

module Field = struct
  type 'a validation = Ok of 'a | Wrong of string | Empty

  type 'a t = {
    id : string;
    elt : Elwd.t;
    value : unit -> 'a;
    validate : 'a -> 'a validation;
  }

  module type S = sig
    type value
    type parameters

    val render : parameters -> Elwd.void_cons
    val get_value : Elwd.t -> value
  end

  let make ?d ?at ?ev (type value p)
      (module Field : S with type parameters = p and type value = value) ~id
      (p : p) ?(validate = fun (v : value) -> Ok v) () =
    let open Lwd_infix in
    let$ elt = Field.render ?d ?at ?ev p () in
    { id; elt; value = (fun () -> Field.get_value elt); validate }

  module Text_input = struct
    type value = string
    type parameters = { placeholder : string }

    let render p ?d ?at:_ =
      Elwd.input ?d ~at:[ `P (At.placeholder (Jstr.v p.placeholder)) ]

    let get_value t =
      let jv = El.to_jv t in
      Jv.get jv "value" |> Jv.to_string
  end

  let text_input = make (module Text_input)
end

type ('t, 'a) form_setter = 't -> 'a Field.validation -> 't

type 'res form_field =
  | F : 'a Field.t * ('form, 'a) form_setter -> 'form form_field

module type Form = sig
  type t

  val default : t
  val fields : t form_field list Lwd.t
end

let create ?d ?at ?ev (type t) (module Form : Form with type t = t) on_submit :
    Elwd.t Lwd.t =
  let open Lwd_infix in
  let$* fields = Form.fields in
  let elts, value =
    List.fold_left fields
      ~init:([], fun () -> Form.default)
      ~f:(fun (elts, form) (F (field, mapper)) ->
        let value () = field.value () |> field.validate in
        (field.elt :: elts, fun () -> mapper (form ()) @@ value ()))
  in
  let on_submit e =
    on_submit @@ value ();
    Ev.prevent_default e
  in
  let on_submit = `P (Elwd.handler Ev.submit on_submit) in
  let elts = List.map elts ~f:(fun e -> `P e) in
  let ev = Option.map_or ~default:[on_submit] (List.cons on_submit) ev in
  Elwd.form ?d ?at ~ev elts

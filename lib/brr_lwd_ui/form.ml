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
    let elt = Field.render ?d ?at ?ev p () in
    Lwd.map elt ~f:(fun elt ->
        let value () = Field.get_value elt in
        { id; elt; value; validate })

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
  val fields : t form_field Lwd.t Lwd_seq.t Lwd.t
end

let create ?d ?at ?ev (type t) (module Form : Form with type t = t) on_submit :
    Elwd.t Lwd.t =
  let fields =
    Lwd_seq.lift Form.fields
    |> Lwd_seq.fold_monoid
         (fun (F (field, mapper)) ->
           ( Lwd_seq.element field.elt,
             let value () = field.value () |> field.validate in
             fun t -> mapper t @@ value () ))
         ( (Lwd_seq.empty, fun _ -> Form.default),
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
  Elwd.form ?d ?at ~ev [ `S elts ]

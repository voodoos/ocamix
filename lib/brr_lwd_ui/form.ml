open Std
open Brrer.Brr
open Brr_lwd

module Field = struct
  type 'a validation = Ok of 'a | Wrong of string | Empty

  type 'a t = {
    id : string option;
    elt : Elwd.t;
    value : Elwd.t -> 'a;
    validate : 'a -> 'a validation;
  }

  module type S = sig
    type value
    type parameters

    val render : parameters -> Elwd.void_cons
    val get_value : Elwd.t -> value
  end

  let make ?d ?at ?ev (type value p)
      (module Field : S with type parameters = p and type value = value) ?id
      (p : p) ?(validate = fun (v : value) -> Ok v) () =
    let elt = Field.render ?d ?at ?ev p () in
    Lwd.map elt ~f:(fun elt ->
        let value elt = Field.get_value elt in
        { id; elt; value; validate })
end

module Text_input = struct
  type value = string
  type parameters = { placeholder : string }

  let render p ?d ?at:_ =
    Elwd.input ?d ~at:[ `P (At.placeholder (Jstr.v p.placeholder)) ]

  let get_value t =
    let jv = El.to_jv t in
    Jv.get jv "value" |> Jv.to_string
end

module Submit = struct
  type value = unit
  type parameters = { text : string }

  let render p ?d ?at:_ =
    Elwd.input ?d
      ~at:[ `P (At.type' (Jstr.v "submit")); `P (At.value (Jstr.v p.text)) ]

  let get_value _ = ()
end

type ('t, 'a) form_setter = 't -> 'a Field.validation -> 't

type 'res form_field =
  | F : 'a Field.t * ('form, 'a) form_setter -> 'form form_field

let make_field ?d ?at ?ev (type value p)
    (module F : Field.S with type parameters = p and type value = value) ?id
    ~(setter : 'f -> value Field.validation -> 'f) (p : p) ?validate () =
  let field = Field.make ?d ?at ?ev (module F) ?id p ?validate () in
  Lwd.map field ~f:(fun field -> F (field, setter))

let text_input ~(setter : 'f -> string Field.validation -> 'f) =
  make_field (module Text_input) ~setter

let submit p = make_field (module Submit) ~setter:(fun t _ -> t) p ()

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
             let value () = field.value field.elt |> field.validate in
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
  Elwd.form ?d ?at ~ev [ `S elts ]

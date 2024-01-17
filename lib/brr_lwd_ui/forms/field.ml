open! Std
open! Brrer.Brr
open! Brr_lwd
module A = Attrs

type 'a validation = Ok of 'a | Error of string | Empty

module type S = sig
  type value
  type parameters

  val render : parameters -> Elwd.t
  val get_value : Elwd.t -> value
end

module Make (Params : S) = struct end

(* A form field is composed of:
   - An html input element with potential validation via attributes
   - A validation function that replace or complete the standard validation
   - A function that retieves the value of the element *)
type 'a t = { elt : Elwd.t; value : unit -> 'a; validate : 'a -> 'a validation }

let get_value t =
  let jv = El.to_jv t in
  Jv.get jv "value"

let make ~(value : Elwd.t -> 'a) ?validate elt =
  Lwd.map elt ~f:(fun elt ->
      let validate = Option.value validate ~default:(fun v -> Ok v) in
      let value () = value elt in
      { elt; value; validate })

let make_input ~(value : Elwd.t -> 'a) ?validate ?d ?(at = []) ?ev
    ?(required = false) ?pattern type' =
  let type' = At.type' (Jstr.v type') in
  let at = `P type' :: at in
  let at =
    at |> A.add_bool At.required required |> A.add_opt At.Name.pattern pattern
  in
  let elt = Elwd.input ?d ~at ?ev () in
  make ~value ?validate elt

let text_input ?validate ?d ?(at = []) ?ev ?required ?pattern ?placeholder
    _value =
  let at = at |> A.add_opt At.Name.placeholder placeholder in
  let value elt = get_value elt |> Jv.to_string in
  make_input ~value ?validate ?d ~at ?ev ?required ?pattern "text"

let password_input ?validate ?d ?(at = []) ?ev ?required ?pattern ?placeholder
    _value =
  let at = at |> A.add_opt At.Name.placeholder placeholder in
  let value elt = get_value elt |> Jv.to_string in
  make_input ~value ?validate ?d ~at ?ev ?required ?pattern "password"

let submit ?d ?(at = []) ?ev text =
  let at = A.add At.Name.value text at in
  make_input ~value:ignore ?d ~at ?ev "submit"

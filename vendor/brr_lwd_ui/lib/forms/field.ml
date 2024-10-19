open! Import
open Brrer.Brr
open! Brr_lwd
module A = Attrs

type 'a validation = Ok of 'a | Error of string | Empty
type label = Elwd.t Elwd.col
type 'a desc = { name : string; default : 'a; label : label }

(* A form field is composed of:
   - An html input element with potential validation via attributes
   - A validation function that replace or complete the standard validation
   - A function that retieves the value of the element *)
type 'a t = {
  elt : Elwd.t Lwd.t;
  value : 'a Lwd.var;
  validate : 'a -> 'a validation;
}

let map_validation ~(f : 'a -> 'b) = function
  | Ok a -> Ok (f a)
  | Error s -> Error s
  | Empty -> Empty

let get_value t =
  let jv = El.to_jv t in
  Jv.get jv "value"

let make_handler ~(value : Jv.t -> 'a) ~(value_change_event : _ Ev.type')
    default_value =
  let var = Lwd.var default_value in
  let on_change =
    Elwd.handler value_change_event (fun ev ->
        let t = Ev.target ev |> Ev.target_to_jv in
        let v = Jv.get t "value" in
        Lwd.set var (value v))
  in
  (on_change, var)

(* TODO: it's probably better to split the validation aprt from the field
   creation. Validation make more sense in the case of a form. There is an
   alternative implementation for the text inputs in `Field_textinput` that should
   be used instead. *)
let make_input ~(value : Jv.t -> 'a) ?validate ?d ?(at = []) ?ev
    ?(required = false) ~value_change_event ?pattern ~type' default_value =
  let type' = At.type' (Jstr.v type') in
  let at = `P type' :: at in
  let at =
    at |> A.add_bool At.required required |> A.add_opt At.Name.pattern pattern
  in
  let validate = Option.value validate ~default:(fun v -> Ok v) in
  let on_change, value =
    (* TODO this does not work very well: sometime the event don't fire and some
       input is missing. *)
    make_handler ~value ~value_change_event default_value
  in
  let ev = `P on_change :: Option.to_list ev in
  let elt = Elwd.input ?d ~at ~ev () in
  { elt; value; validate }

let text_input ?validate ?d ?(at = []) ?ev ?required ?pattern ?placeholder
    default_value =
  let at =
    at
    |> A.add_opt At.Name.placeholder placeholder
    |> A.add_opt At.Name.value default_value
  in
  let default_value = Option.get_or ~default:"" default_value in
  let value = Jv.to_string in
  make_input ~value ?validate ?d ~at ?ev ?required ?pattern
    ~value_change_event:Ev.input ~type':"text" default_value

let password_input ?validate ?d ?(at = []) ?ev ?required ?pattern ?placeholder
    () =
  let at = at |> A.add_opt At.Name.placeholder placeholder in
  let value = Jv.to_string in
  make_input ~value ~value_change_event:Ev.input ?validate ?d ~at ?ev ?required
    ?pattern ~type':"password" ""

let submit ?d ?(at = []) ?ev text =
  let at = A.add At.Name.value text at in
  (* TODO this should be more precise. Submit inputs are different. *)
  make_input ~value:ignore ~value_change_event:Ev.change ?d ~at ?ev
    ~type':"submit" ()

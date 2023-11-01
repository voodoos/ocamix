open! Std
open Brr
open Brr_lwd

module Make (Properties : sig
  val base_classes : Classes.t
end) =
struct
  let make ?d ?(classes = Classes.Add []) ~on_click content =
    let open Lwd_infix in
    let _classes = Classes.update Properties.base_classes classes in
    let$ button = Elwd.button ?d content in
    let listener = Ev.listen Ev.click on_click (El.as_target button) in
    (button, listener)

  let make_pure ?d ?(classes = Classes.Add []) ~on_click content =
    let _classes = Classes.update Properties.base_classes classes in
    let button = El.button ?d content in
    let listener = Ev.listen Ev.click on_click (El.as_target button) in
    (button, listener)
end

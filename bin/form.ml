open! Std
open Brr_lwd_ui.Form

module My_form = struct
  type t = { name : string Field.validation; age : int Field.validation }

  let default = { name = Empty; age = Empty }

  let fields =
    let open Lwd_infix in
    let$ name_input =
      Field.text_input ~id:"name" { placeholder = "again" } ()
    in
    [ F (name_input, fun t v -> { t with name = v }) ]
end

let my_form = create (module My_form)

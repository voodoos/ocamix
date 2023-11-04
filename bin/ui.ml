open Brr_lwd_ui

module Two_state_button = struct
  let at = function
    | Button.Two_state.On -> [ `P (Classes.at_of_string "on") ]
    | Off -> [ `P (Classes.at_of_string "off") ]

  let make ~on_click =
    let on_click = Button.handler_with_state Brr.Ev.click on_click in
    Button.two_state ~at ~ev:[ `P on_click ]
end

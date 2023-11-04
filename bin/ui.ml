open Brr_lwd_ui

module Two_state_button = struct
  let base = Attrs.classes [ "button" ]

  let at = function
    | Button.On -> Attrs.classes [ "on" ]
    | Off -> Attrs.classes [ "off" ]

  let make ~on_click =
    let on_click = Button.handler Brr.Ev.click on_click in
    Button.two_state ~base ~at ~ev:[ `P on_click ]
end

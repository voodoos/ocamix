open Brr_lwd_ui

module Two_state_button = Button.Two_state (struct
  let base_classes = Classes.make [ "pouet_base"; "another" ]
  let on_classes = Classes.make [ "on" ]
  let off_classes = Classes.make [ "off" ]
end)

module Button = Button.Make (struct
  let base_classes = Classes.make [ "pouet_base"; "another" ]
end)

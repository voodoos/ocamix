open Brr_lwd_ui

module Button = Button.Make (struct
  let base_classes = Classes.make [ "pouet_base" ]
end)

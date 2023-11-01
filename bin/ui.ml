open Brr_lwd_ui

module Two_state_button = struct
  include Button.Two_state

  let config =
    {
      base_classes = Classes.make [ "pouet_base"; "another" ];
      on_classes = Classes.make [ "on" ];
      off_classes = Classes.make [ "off" ];
    }

  let make = make ~config
end

module Button = struct
  let base_classes = Classes.make [ "pouet_base"; "another" ]
  let make = Button.Simple.make ~base_classes
end

open! Std
open Brrer.Brr
open Brr_lwd
open Brr_lwd_ui

let char_ham = "☰"

let make ?at:_ ?ev () =
  let at = Attrs.classes [ "hover-menu" ] in
  Elwd.div ~at:(Attrs.to_at at) ?ev
    [ `P (El.txt' char_ham); `R (Elwd.menu [ `P (El.txt' "menu item") ]) ]

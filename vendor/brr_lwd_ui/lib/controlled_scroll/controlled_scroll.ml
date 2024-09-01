open Import
open Brr
open Brr_lwd

type target = Pos of int | El of Elwd.t
type _t = { elt : Elwd.t Lwd.t; scroll_position : target option Lwd.var }

(* TODO: make a proper binding *)
let js_scroll elt target =
  match target with
  | Pos x ->
      let elt = El.to_jv elt in
      ignore @@ Jv.call elt "scroll" [| Jv.of_int 0; Jv.of_int x |]
  | El el -> El.scroll_into_view el

let make ?(at = []) ?(ev = []) ?on_create ~scroll_target elt =
  let active = Lwd.var true in
  let active_class =
    Lwd.map (Lwd.get active) ~f:(function
      | false -> Attrs.A At.void
      | true -> Attrs.A (At.class' (Jstr.v "locked")))
  in
  let at =
    Attrs.O.(
      `P (C "lwdui-controlled-scroll-wrapper") @:: `R active_class @:: at)
  in
  let controls =
    let at = Attrs.class_ (`P "lwdui-controlled-scroll-controls") in
    let ev =
      let on_click _ = Lwd.set active true in
      let handler = Elwd.handler Ev.click on_click in
      [ `P handler ]
    in
    Elwd.div ~at ~ev [ `R (Elwd.button [ `P (El.txt' "Show playing") ]) ]
  in
  let on_wheel =
    Elwd.handler Ev.wheel (fun _ev ->
        if Lwd.peek active then Lwd.set active false)
  in
  let elt =
    let state = Utils.triple elt (Lwd.get active) scroll_target in
    Lwd.map state ~f:(fun (elt, active, pos) ->
        if active then Option.iter (js_scroll elt) pos;
        elt)
  in
  Elwd.div ~at ~ev:(`P on_wheel :: ev) ?on_create [ `R elt; `R controls ]

open! Std
open Brr
open Brr_lwd
open! Lwd_infix

let ui =
  let playlist = Lwd.var 0 in
  let on_click _ _ =
    Lwd.set playlist (Lwd.peek playlist + 1);
    Ui.Two_state_button.Toggle
  in
  let { Ui.Two_state_button.elt = btn_mix; _ } =
    Ui.Two_state_button.make ~on_click
      [
        `R
          (Lwd.map (Lwd.get playlist) ~f:(fun pl ->
               El.txt' ("click" ^ string_of_int pl)));
      ]
  in
  Elwd.div
    [
      `P (El.txt' "Click the button");
      `P (El.br ());
      `R btn_mix;
      `R
        (Elwd.p
           [
             `R
               (Lwd.map (Lwd.get playlist) ~f:(fun v ->
                    El.txt' (string_of_int v)));
           ]);
    ]

let _ =
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    Console.(log [ str "on invalidate" ]);
    ignore @@ G.request_animation_frame
    @@ fun _ -> ignore @@ Lwd.quick_sample ui
  in
  let on_load _ =
    Console.(log [ str "on load" ]);
    El.append_children (Document.body G.document) [ Lwd.quick_sample ui ];
    Lwd.set_on_invalidate ui on_invalidate
  in
  ignore (Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window))

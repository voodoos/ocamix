open Brr
open Brr_lwd

let ui =
  let playlist = Lwd.var 0 in
  (* let _btn_mix = El.button  *)
  let btn_mix = El.button [ El.txt' "click" ] in
  let _ =
    Ev.listen Ev.click
      (fun _ -> Lwd.set playlist (Lwd.peek playlist + 1))
      (El.as_target btn_mix)
  in
  Elwd.div
    [
      `P (El.txt' "Click the button");
      `P (El.br ());
      `P btn_mix;
      `R
        (Elwd.p
           [
             `R
               (Lwd.map (Lwd.get playlist) ~f:(fun v ->
                    El.txt' (string_of_int v)));
           ]);
    ]

let () =
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    Console.(log [ str "on invalidate" ]);
    let _ : int =
      G.request_animation_frame @@ fun _ ->
      let _ui = Lwd.quick_sample ui in
      (*El.set_children (Document.body G.document) [ui]*)
      ()
    in
    ()
  in
  let on_load _ =
    Console.(log [ str "onload" ]);
    El.append_children (Document.body G.document) [ Lwd.quick_sample ui ];
    Lwd.set_on_invalidate ui on_invalidate
  in
  ignore (Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window))

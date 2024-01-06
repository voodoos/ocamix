open Import
open Brr
open Brr_lwd

module Db_worker = Db.Worker_api.Start_client (struct
  let url = "./db_worker.bc.js"
end)

let app _idb =
  let sync_progress = Lwd.var { Db.Sync.remaining = 0 } in
  let ui_progress =
    let open Lwd_infix in
    let$ { remaining } = Lwd.get sync_progress in
    let txt = Format.sprintf "Remaining sync queries: %i" remaining in
    El.txt' txt
  in
  let playlist = Brr_lwd_ui.Persistent.var ~key:"toto1" 0 in
  let on_click _ _ =
    Lwd.set playlist (Lwd.peek playlist + 1);
    Lang.set Lang.En;
    Brr_lwd_ui.Button.Next
  in
  let btn_mix, _, _ =
    Ui.Two_state_button.make ~on_click (fun _ ->
        [
          `R
            (Lwd.map (Lwd.get playlist) ~f:(fun pl ->
                 El.txt' ("click" ^ string_of_int pl)));
        ])
  in
  Elwd.div
    [
      `R ui_progress;
      `P (El.br ());
      `R (Lang._s "click" El.txt);
      `P (El.br ());
      `R btn_mix;
      `R
        (Elwd.p
           [
             `R
               (Lwd.map (Lwd.get playlist) ~f:(fun v ->
                    El.txt' (string_of_int v)));
           ]);
      `P (El.br ());
      `R (Menu.make ());
      `R
        (Ui_playlist.make ~total:50
           ~fetch:(fun i -> Db_worker.(query (Get i)))
           ());
    ]

let _ =
  let on_load _ =
    Db.with_idb ~name:"tracks" ~version:1 @@ fun idb ->
    let app = Lwd.observe (app idb) in
    let on_invalidate _ =
      ignore @@ G.request_animation_frame
      @@ fun _ -> ignore @@ Lwd.quick_sample app
    in
    El.append_children (Document.body G.document) [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

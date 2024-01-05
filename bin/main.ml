open Import
open Brr
open Brr_lwd

module Db_worker = Lib.Db_worker_api.Client (struct
  let url = "./db_worker.bc.js"
end)

let _db =
  let open Data_source.Jellyfin in
  let base_url = "http://localhost:8096" in
  let username = "root" in
  let pw = "rootlocalroot" in
  Brr_lwd_ui.Persistent.var_fut ~key:"v_connexion" (fun () ->
      connect ~base_url Api.Authenticate_by_name.{ username; pw })

let app _idb =
  let sync_progress = Lwd.var { Db.Sync.remaining = 0 } in
  let ui_progress =
    let open Lwd_infix in
    let$ { remaining } = Lwd.get sync_progress in
    let txt = Format.sprintf "Remaining sync queries: %i" remaining in
    El.txt' txt
  in
  let items = Lwd.var [] in
  let _ =
    let open Fut.Result_syntax in
    Console.log [ "worker query"; Lib.Db_worker_api.Queries.Get_all () ];
    let+ result = Db_worker.(query (Get_all ())) in
    Lwd.set items result
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
  let img_url id =
    Printf.sprintf "http://localhost:8096/Items/%s/Images/Primary" id
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
           ~render:(fun i { name; album_id; _ } ->
             [
               El.div [ El.txt' (string_of_int i) ];
               El.div
                 [
                   El.img
                     ~at:[ At.src (Jstr.v @@ img_url album_id); At.width 40 ]
                     ();
                 ];
               El.div [ El.txt' name ];
             ])
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

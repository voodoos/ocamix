open Import
open Brr
open Brr_lwd

module Db_worker = Db.Worker_api.Start_client (struct
  let url = "./db_worker.bc.js"
end)

let fetch view i = Db_worker.(query (Get (view, i)))

let connexion =
  let source, set_source = Fut.create () in
  let _ =
    let open Data_source.Jellyfin in
    let base_url = "http://localhost:8096" in
    let username = "root" in
    let password = "rootlocalroot" in
    let open Fut.Syntax in
    let+ source = connect { base_url; username; password } in
    set_source source
  in
  source

let servers =
  let open Fut.Syntax in
  let+ connexion = connexion in
  let open Result.Infix in
  let+ connexion = Result.map_err (fun e -> `Jv e) connexion in
  [ (connexion.auth_response.server_id, connexion) ]

module Player = Player.Playback_controller (struct
  let fetch = fetch
  let servers = servers
end)

let app _idb =
  let open Fut.Result_syntax in
  let+ servers = servers in
  let _ = Db_worker.query @@ Servers servers in
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
  let main_view = Db_worker.query (Create_view Db.View.(req ())) in
  let player = Player.make () in
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
      (* `R (Menu.make ()); *)
      `R player;
      `R
        (Ui_playlist.make ~reset_playlist:Player.reset_playlist ~servers ~fetch
           player main_view);
    ]

let is_storage_persistent =
  Brr_io.Storage.(manager G.navigator |> Manager.persist)

let _ =
  let on_load _ =
    Console.log [ "Persist ?"; is_storage_persistent ];
    Db.with_idb ~name:"tracks" ~version:1 @@ fun idb ->
    let open Fut.Result_syntax in
    ignore
    @@
    let+ app = app idb in
    let app = Lwd.observe app in
    let on_invalidate _ =
      ignore @@ G.request_animation_frame
      @@ fun _ -> ignore @@ Lwd.quick_sample app
    in
    El.append_children (Document.body G.document) [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

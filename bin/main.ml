open Import
open Brr
open Brr_lwd

module Db_worker = Db.Worker_api.Start_client (struct
  let url = "./db_worker.bc.js"
end)

let _ =
  Db_worker.listen Servers_status_update ~f:(fun v ->
      Console.log [ "listened"; v ])

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
  let status = Lwd.var Db.Sync.initial_report in
  [ (connexion.auth_response.server_id, { Servers.connexion; status }) ]

module P = Player.Playback_controller (struct
  let fetch = fetch
  let servers = servers
end)

let app _idb =
  let open Fut.Result_syntax in
  let+ servers = servers in
  let _ =
    let servers =
      List.map servers ~f:(fun (id, s) -> (id, s.Servers.connexion))
    in
    Db_worker.query @@ Add_servers servers
  in
  let playlist = Brr_lwd_ui.Persistent.var ~key:"toto1" 0 in
  let on_click _ _ =
    Lwd.set playlist (Lwd.peek playlist + 1);
    Lang.set Lang.En;
    Brr_lwd_ui.Button.Next
  in
  let _btn_mix, _, _ =
    Ui.Two_state_button.make ~on_click (fun _ ->
        [
          `R
            (Lwd.map (Lwd.get playlist) ~f:(fun pl ->
                 El.txt' ("click" ^ string_of_int pl)));
        ])
  in
  let main_view = Db_worker.query (Create_view Db.View.(req ())) in
  let player = P.make () in
  let status =
    El.div
      ~at:[ At.style (Jstr.v "grid-column:1/-1") ]
      [ El.h1 [ El.txt' "Welcome to OCAMIX" ] ]
  in
  let player_ui =
    Elwd.div
      ~at:
        [
          `P (At.style (Jstr.v "grid-column:1/-1"));
          `P (At.class' (Jstr.v "player-wrapper"));
        ]
      [ `R player ]
  in
  let main_list =
    Ui_playlist.make ~reset_playlist:P.reset_playlist ~servers ~fetch player
      main_view
  in
  let now_playing =
    Lwd.bind (Lwd.get Player.playstate) ~f:(function
      | None -> Elwd.div []
      | Some { playlist; _ } ->
          Ui_playlist.make ~reset_playlist:P.reset_playlist ~servers ~fetch
            player (Fut.ok playlist))
  in
  Elwd.div
    ~at:Brr_lwd_ui.Attrs.(to_at ~id:"main-layout" @@ classes [])
    [ `P status; `R main_list; `R now_playing; `R player_ui ]

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

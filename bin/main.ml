open Import
open Brr

let fetch view i = Worker_client.(query (Get (view, i)))

module P = Player.Playback_controller (struct
  let fetch = fetch
end)

let app _idb =
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
  let main_view = Worker_client.query (Create_view Db.View.(req ())) in
  let player = P.make () in
  let status =
    Elwd.div
      ~at:[ `P (At.style (Jstr.v "grid-column:1/-1")) ]
      [ `P (El.h1 [ El.txt' "Welcome to OCAMIX" ]); `R (Servers.ui ()) ]
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
    Ui_playlist.make ~reset_playlist:P.reset_playlist ~fetch player main_view
  in
  let now_playing =
    Lwd.bind (Lwd.get Player.playstate) ~f:(function
      | None -> Elwd.div []
      | Some { playlist; _ } ->
          Ui_playlist.make ~reset_playlist:P.reset_playlist ~fetch player
            (Fut.ok playlist))
  in
  Elwd.div
    ~at:Brr_lwd_ui.Attrs.(to_at ~id:"main-layout" @@ classes [])
    [ `R status; `R main_list; `R now_playing; `R player_ui ]

let is_storage_persistent =
  Brr_io.Storage.(manager G.navigator |> Manager.persist)

let _ =
  let on_load _ =
    Console.log [ "Persist ?"; is_storage_persistent ];
    Db.with_idb ~name:"tracks" ~version:1 @@ fun idb ->
    ignore
    @@
    let app = Lwd.observe @@ app idb in
    let on_invalidate _ =
      ignore @@ G.request_animation_frame
      @@ fun _ -> ignore @@ Lwd.quick_sample app
    in
    El.append_children (Document.body G.document) [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

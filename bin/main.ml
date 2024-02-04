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
  let player = P.make () in
  let status =
    Elwd.div
      ~at:[ `P (At.style (Jstr.v "grid-column:1/-1")) ]
      [ `P (El.h1 [ El.txt' "Welcome to OCAMIX" ]); `R (Servers.ui ()) ]
  in
  let player_ui =
    Elwd.div ~at:[ `P (At.style (Jstr.v "grid-column:1/-1")) ] [ `R player ]
  in
  let libraries =
    let var = Lwd.var [] in
    let _ =
      let open Fut.Result_syntax in
      let+ libraries = Servers.servers_libraries () in
      let list =
        String.Items_MultiMap.fold libraries [] (fun acc _ { item; _ } ->
            item :: acc)
      in
      Lwd.set var list
    in
    var
  in
  let filters, f_value =
    let f_libraries =
      let open Brr_lwd_ui.Field_checkboxes in
      let choices =
        Lwd.map (Lwd.get libraries) ~f:(fun l ->
            Console.log [ "Libraries:"; l ];
            Lwd_seq.transform_list l (fun l ->
                Lwd_seq.element @@ Check (l.id, [ `P (El.txt' l.name) ], true)))
      in
      make { name = "pouet"; desc = choices }
    in
    (f_libraries.field, f_libraries.value)
  in
  let main_view =
    Lwd.map f_value ~f:(fun l ->
        Worker_client.query
          (Create_view Db.View.(req Audio ~src_views:(Only l) ~sort:Random ())))
  in

  let main_list =
    Lwd.map main_view ~f:(fun main_view ->
        Ui_playlist.make ~reset_playlist:P.reset_playlist ~fetch player
          main_view)
  in
  let now_playing =
    (*todo: do we need that join ?*)
    Lwd.bind (Lwd.get Player.playstate) ~f:(function
      | None -> Elwd.div []
      | Some { playlist; _ } ->
          Ui_playlist.make ~reset_playlist:P.reset_playlist ~fetch player
            (Fut.ok playlist))
  in
  Elwd.div
    ~at:Brr_lwd_ui.Attrs.(to_at ~id:"main-layout" @@ classes [])
    [
      `R status;
      `R
        (Elwd.div
           ~at:[ `P (At.class' (Jstr.v "item-list")) ]
           [
             `R filters;
             `R ((*todo: do we need that join ?*) Lwd.join main_list);
           ]);
      `R now_playing;
      `R player_ui;
      `P (El.div [ El.txt' "icons by icons8" ]);
    ]

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

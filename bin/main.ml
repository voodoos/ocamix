open Import
open Brr

let () = Random.self_init ()

(* The session uuid is stored to the local storage and used to identify a user
   session. This is required by Jellyfin authorization scheme. *)
(* The official client does it like this:

   function generateDeviceId() {
       const keys = [];

       keys.push(navigator.userAgent);
       keys.push(new Date().getTime());
       if (window.btoa) {
           return btoa(keys.join('|')).replaceAll('=', '1');
       }

       return new Date().getTime();
   }
*)
let session_uuid =
  (* We never react to this var we could replace it *)
  Brr_lwd_ui.Persistent.var_f ~key:"session_uuid" (fun () ->
      Std.new_uuid_v4 () |> Uuidm.to_string)

let _ =
  let uuid = Lwd.peek session_uuid in
  Data_source.Jellyfin_api.set_session_uuid uuid;
  Worker_client.query (Set_session_uuid (Lwd.peek session_uuid))

let fetch ranged_view i =
  Worker_client.(query (Get (ranged_view.View.view, ranged_view.order, i)))

module P = Player.Playback_controller (struct
  let fetch = fetch
end)

let fetch ranged_view i =
  let data = fetch ranged_view i in
  Array.mapi i ~f:(fun i _ ->
      let open Fut.Result_syntax in
      let* data = data in
      match data.(i) with
      | Some v -> Fut.ok v
      | None -> Fut.error (`Msg "No result"))

let app (db : Brr_io.Indexed_db.Database.t) =
  let status =
    Elwd.div
      ~at:[ `P (At.style (Jstr.v "grid-column:1/-1")) ]
      [ `P (El.h1 [ El.txt' "Welcome to OCAMIX" ]); `R (Servers.ui ()) ]
  in
  let player_ui =
    let player = P.make db () in
    Elwd.div ~at:[ `P (At.style (Jstr.v "grid-column:1/-1")) ] [ `R player ]
  in
  let main_view =
    (* TODO this is silly *)
    let view = Lwd.get Ui_filters.view in
    let request = Lwd.map view ~f:(fun { View.request; _ } -> request) in
    let item_count =
      Lwd.map view ~f:(fun { View.item_count; _ } -> item_count)
    in
    let order =
      Lwd.map2 item_count (Lwd.get Ui_filters.selected_order) ~f:(fun size ->
          View.Order.of_string ~size)
    in
    { Lwd_view.request; item_count; start_offset = Lwd.pure 0; order }
  in
  (* TODO filter and view does not update correctly while syncing *)
  let main_list =
    Ui_playlist.make ~reset_playlist:P.reset_playlist ~fetch main_view
  in
  let now_playing =
    let playlist =
      Lwd.map (Lwd.get Player.playstate.playlist) ~f:(function
        | None -> Elwd.span [ `P (El.txt' "Nothing playing") ]
        | Some playlist ->
            let view =
              {
                Lwd_view.request = Lwd.pure playlist.view.request;
                item_count = Lwd.pure playlist.view.item_count;
                start_offset = Lwd.pure playlist.view.start_offset;
                order = Lwd.pure playlist.order;
              }
            in
            Ui_playlist.make_now_playing ~reset_playlist:P.reset_playlist ~fetch
              view)
    in
    (*todo: do we need that join ?*)
    Lwd.join playlist
  in
  let big_cover = Ui_kiosk.big_cover in
  Elwd.div
    ~at:Brr_lwd_ui.Attrs.(to_at ~id:"main-layout" @@ classes [])
    [
      `R status;
      `R
        (Elwd.div
           ~at:[ `P (At.class' (Jstr.v "item-list")) ]
           [ `R big_cover; `R Ui_filters.bar; `R (Elwd.div [ `R main_list ]) ]);
      `R
        (Elwd.div ~at:[ `P (At.class' (Jstr.v "playlist")) ] [ `R now_playing ]);
      `R player_ui;
    ]

let is_storage_persistent =
  Brr_io.Storage.(manager G.navigator |> Manager.persist)

let _ =
  let on_load _ =
    Console.log [ "Persist ?"; is_storage_persistent ];
    Db.with_idb (fun idb ->
        let app = Lwd.observe @@ app idb in
        let on_invalidate _ =
          ignore @@ G.request_animation_frame
          @@ fun _ -> ignore @@ Lwd.quick_sample app
        in
        El.append_children (Document.body G.document) [ Lwd.quick_sample app ];
        Lwd.set_on_invalidate app on_invalidate)
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

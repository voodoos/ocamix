open Import
open Brr

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

let app =
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
  let status =
    Elwd.div
      ~at:[ `P (At.style (Jstr.v "grid-column:1/-1")) ]
      [ `P (El.h1 [ El.txt' "Welcome to OCAMIX" ]); `R (Servers.ui ()) ]
  in
  let player_ui =
    let player = P.make () in
    Elwd.div ~at:[ `P (At.style (Jstr.v "grid-column:1/-1")) ] [ `R player ]
  in
  let f_search =
    let open Brr_lwd_ui.Field_textinput in
    make { name = "pouet"; default = None; label = [] }
  in
  let f_sort =
    let open Brr_lwd_ui.Field_select in
    let options =
      Lwd.pure
        (Lwd_seq.of_list [ ("date_added", "Date added"); ("name", "Name") ])
    in
    make { name = "view-sort"; default = "date_added"; label = [] } options
  in
  let f_order =
    let open Brr_lwd_ui.Field_select in
    let options =
      Lwd.pure
        (Lwd_seq.of_list
           [ ("asc", "Asc"); ("desc", "Desc"); ("random", "Random") ])
    in
    make { name = "view-order"; default = "desc"; label = [] } options
  in
  let f_sort_order = Lwd.pair f_sort.value f_order.value in
  let filters, f_value =
    let f_libraries =
      let open Brr_lwd_ui.Field_checkboxes in
      let choices =
        Lwd_seq.fold_monoid
          (fun (_, l) ->
            Lwd_seq.map
              (fun (l : Db.Stores.Items.t) ->
                Check (l.item.id, [ `P (El.txt' l.item.name) ], true))
              l)
          (Lwd.return Lwd_seq.empty, Lwd.map2 ~f:Lwd_seq.concat)
          Servers.servers_libraries
      in
      make { name = "pouet"; desc = Lwd.join choices }
    in
    let filters =
      Elwd.div
        [
          `R f_sort.field;
          `R f_order.field;
          `R f_search.field;
          `R f_libraries.field;
        ]
    in
    (filters, f_libraries.value)
  in
  let main_view =
    let previous_value = ref None in
    let request =
      Ui_utils.map3 f_value f_search.value f_sort_order ~f:(fun l t (s, o) ->
          let filters = Option.map (fun s -> [ Db.View.Search s ]) t in
          Console.log
            [
              "Updating main view:";
              Jv.of_option ~none:(Jv.of_string "\"\"") Jv.of_string t;
              (* Jv.of_list Jv.of_string l; *)
              Jv.of_string s;
            ];
          let open Fut.Result_syntax in
          let sort = Db.View.Sort.of_string s in
          let open Fut.Result_syntax in
          let new_view =
            Db.View.(
              req Audio ~src_views:(Only (Lwd_seq.to_list l)) ~sort ?filters ())
          in
          Option.map_or ~default:new_view
            (fun old ->
              if Equal.poly old new_view then old
              else (
                previous_value := Some new_view;
                new_view))
            !previous_value)
    in
    let item_count =
      Lwd.map request ~f:(fun req -> Worker_client.get_view_item_count req)
    in
    let item_count = (* FIXME *) Lwd.join item_count in
    let order =
      Lwd.map2 item_count f_sort_order ~f:(fun item_count (_, order) ->
          let size = item_count in
          let order = View.Order.of_string ~size order in
          order)
    in
    { Lwd_view.request; item_count; start_offset = Lwd.pure 0; order }
  in

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
  let big_cover =
    let open Attrs.O in
    let display_none =
      Lwd.map (Lwd.get App_state.active_layout) ~f:(function
        | Main -> At.class' (Jstr.v "display-none")
        | Kiosk -> At.void)
    in
    let style =
      Lwd.map (Lwd.get Player.now_playing) ~f:(fun np ->
          let src =
            match np with
            | None -> "track.png"
            | Some { item = { id; album_id; server_id; _ }; _ } ->
                let image_id = Option.value ~default:id album_id in
                let servers = Lwd_seq.to_list (Lwd.peek Servers.connexions) in
                let connexion : DS.connexion = List.assq server_id servers in
                (* todo: this is done in multiple places, we should factor
                   that out. *)
                Printf.sprintf "%s/Items/%s/Images/Primary?width=800&format=Jpg"
                  connexion.base_url image_id
          in
          At.style (Jstr.v (Printf.sprintf "background-image: url(%S)" src)))
    in
    let at = [ `R display_none; `P (At.class' (Jstr.v "big-cover")) ] in
    Elwd.div ~at [ `R (Elwd.div ~at:[ `R style ] []) ]
  in
  Elwd.div
    ~at:Brr_lwd_ui.Attrs.(to_at ~id:"main-layout" @@ classes [])
    [
      `R status;
      `R
        (Elwd.div
           ~at:[ `P (At.class' (Jstr.v "item-list")) ]
           [ `R big_cover; `R filters; `R main_list ]);
      `R
        (Elwd.div ~at:[ `P (At.class' (Jstr.v "playlist")) ] [ `R now_playing ]);
      `R player_ui;
    ]

let is_storage_persistent =
  Brr_io.Storage.(manager G.navigator |> Manager.persist)

let _ =
  let on_load _ =
    Console.log [ "Persist ?"; is_storage_persistent ];
    let app = Lwd.observe @@ app in
    let on_invalidate _ =
      ignore @@ G.request_animation_frame
      @@ fun _ -> ignore @@ Lwd.quick_sample app
    in
    El.append_children (Document.body G.document) [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

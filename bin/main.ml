open Import
open Brr
open Brr_lwd
module OI = Db.Stores.Orderred_items_store
module I = Db.Stores.Items_store

let with_idb ?version ~name f =
  let open Brr_io.Indexed_db in
  let f _ev dbr =
    let db = Request.result dbr in
    f db
  in
  get_factory ()
  |> Factory.open' ~name ?version
  |> Open_db_request.on_upgrade_needed ~f:(fun e q ->
         let old_version, new_version =
           let v = Ev.as_type e in
           Events.Version_change.(old_version v, new_version v)
         in
         Console.info
           [
             "Upgrading indexed_db schema from version";
             old_version;
             "to";
             new_version;
           ];
         let db = Request.result q in
         let list =
           Database.create_object_store (module OI) ~auto_increment:false db
         in
         let items =
           Database.create_object_store (module I) ~auto_increment:false db
         in
         Console.info [ "Stores created:"; list; items ])
  |> Request.on_success ~f |> ignore

let db ?progress_var idb =
  let open Fut.Result_syntax in
  let open Data_source.Jellyfin in
  let base_url = "http://localhost:8096" in
  let+ connexion =
    Brr_lwd_ui.Persistent.var_fut ~key:"v_connexion" (fun () ->
        connect ~base_url
          Api.Authenticate_by_name.{ username = "root"; pw = "rootlocalroot" })
  in
  let source = Lwd.peek connexion in
  let report = Option.map (fun v p -> Lwd.set v p) progress_var in
  Db.Sync.check_and_sync ?report ~source idb

let app idb =
  let sync_progress = Lwd.var { Db.Sync.remaining = 0 } in
  let () = ignore @@ db ~progress_var:sync_progress idb in
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
    ]

let _ =
  let on_load _ =
    with_idb ~name:"tracks" ~version:1 @@ fun idb ->
    let app = Lwd.observe (app idb) in
    let on_invalidate _ =
      ignore @@ G.request_animation_frame
      @@ fun _ -> ignore @@ Lwd.quick_sample app
    in
    El.append_children (Document.body G.document) [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

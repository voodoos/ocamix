open! Std
open Brr
open Brr_lwd
open! Lwd_infix

let ui =
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
  let shared_drag_data = Lwd.var None in
  let columns = Lwd.var Lwd_seq.empty in
  let list, _tbl = Ui.draggable_table () in
  let form =
    Form.my_form (fun form ->
        Console.log [ "a"; form; form.name ];
        match form.name with
        | Ok _name ->
            let list, _tbl = Ui.draggable_table ~shared_drag_data () in
            Lwd.set columns
            @@ Lwd_seq.concat (Lwd.peek columns) (Lwd_seq.element list)
        | _ -> ())
  in
  let el_columns =
    let columns = Lwd.get columns in
    Lwd_seq.lift columns
  in
  Elwd.div
    [
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
      `R list;
      `P (El.br ());
      `R (Menu.make ());
      `P (El.br ());
      `R form;
      `P (El.br ());
      `S el_columns;
    ]

module OI = Db.Stores.Orderred_items_store

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
         Console.log [ "pouet"; db ];
         let store =
           Database.create_object_store (module OI) ~auto_increment:true db
         in
         Console.log [ "pouet"; store ])
  |> Request.on_success ~f |> ignore

let db idb =
  let open Fut.Result_syntax in
  let open Data_source.Jellyfin in
  let base_url = "http://localhost:8096" in
  let+ connexion =
    Brr_lwd_ui.Persistent.var_fut ~key:"v_connexion" (fun () ->
        connect ~base_url
          Api.Authenticate_by_name.{ username = "root"; pw = "rootlocalroot" })
  in
  let source = Lwd.peek connexion in
  Db.Sync.check_and_sync ~source idb

let _ =
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    Console.(log [ str "on invalidate" ]);
    ignore @@ G.request_animation_frame
    @@ fun _ -> ignore @@ Lwd.quick_sample ui
  in
  let on_load _ =
    Console.(log [ str "on load" ]);
    with_idb ~name:"tracks" ~version:1 @@ fun idb ->
    ignore @@ db idb;
    El.append_children (Document.body G.document) [ Lwd.quick_sample ui ];
    Lwd.set_on_invalidate ui on_invalidate
  in
  ignore (Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window))

open! Import
open Brr
open Brr_lwd

type playstate = {
  playlist : Db.View.ranged option Lwd.var;
  current_index : int Lwd.var;
}

type t = Elwd.t Lwd.t

let playstate = { playlist = Lwd.var None; current_index = Lwd.var 0 }

type now_playing = {
  item : Db.Generic_schema.Track.Key.t * Db.Generic_schema.Track.t;
  url : string;
}

let now_playing = Lwd.var None

(* see https://github.com/jellyfin/jellyfin/blob/4786901bb796c3e912f13b686571fde8d16f49c5/tests/Jellyfin.Model.Tests/Test%20Data/DeviceProfile-Firefox.json *)

(** Playback issue with alac files (and probably all codecs non-natively
    supported by the browser)
    - Some clients (sonixd) give up, mpv based players work.
    - The official client manage to play the file but gives a blob to the audio
      player instead of a hls url as it does for other files.
    - Jellyfin should use the device id and associated capabilities to
      automatically transcode, shouldn't it ? *)
let audio_url (server : DS.connexion) item_id =
  Printf.sprintf
    "%s/Audio/%s/universal?api_key=%s&audioCodec=aac&container=opus,mp3,aac,m4a,m4b,flac,wav,ogg&transcodingContainer=ts&transcodingProtocol=hls"
    server.base_url item_id server.auth_response.access_token

let idb =
  let idb, set_idb = Fut.create () in
  let _ = Db.with_idb @@ fun idb -> ignore (set_idb idb) in
  idb

let get_album_cover_link_opt ~base_url ~size ~album_id ~cover_type =
  (* Todo for better user experience we should pre-fetch the images before
     updating the DOM. This is especially true for back covers that come from
     the coverart archive which can be slow to download. *)
  match album_id with
  | None -> Fut.return None
  | Some album_id ->
      let open Brr_io.Indexed_db in
      let open Db.Stores in
      let open Db.Generic_schema in
      let open Fut.Syntax in
      let* idb = idb in
      let index =
        Database.transaction [ (module Albums_store) ] ~mode:Readonly idb
        |> Transaction.object_store (module Albums_store)
        |> Albums_store.index (module Albums_by_idx) ~name:"by-idx"
      in
      let+ album_id = Albums_by_idx.get album_id index |> Request.fut_exn in
      Option.bind album_id (fun { Album.id = Id.Jellyfin id; mbid; _ } ->
          if Equal.poly cover_type App_state.Front then
            Some
              (Printf.sprintf "%s/Items/%s/Images/Primary?width=%i&format=Jpg"
                 base_url id size)
          else
            Option.map
              (fun mbid ->
                Printf.sprintf
                  "https://coverartarchive.org/release/%s/back-1200" mbid)
              mbid)

let get_album_cover_link ~base_url ~size ~album_id ~cover_type =
  get_album_cover_link_opt ~base_url ~size ~album_id ~cover_type
  |> Fut.map (Option.value ~default:"track.png")

let cover_var ~base_url ~size ~album_id ~cover_type =
  Brr_lwd_ui.Utils.var_of_fut ~init:"track.png"
  @@ get_album_cover_link ~base_url ~size ~album_id ~cover_type

module Playback_controller (P : sig
  val fetch :
    View.ranged ->
    int array ->
    ( (Db.Generic_schema.Track.Key.t * Db.Generic_schema.Track.t) option array,
      Db.Worker_api.error )
    Fut.result
end) =
struct
  let set_play_url playlist current_index =
    match playlist with
    | None -> Fut.ok ()
    | Some playlist ->
        let open Db.Generic_schema in
        let open Fut.Result_syntax in
        let+ item =
          let+ result = P.fetch playlist [| current_index |] in
          match result with
          | [|
           Some
             Track.(
               ( { Key.name; _ },
                 {
                   id = Jellyfin id;
                   server_id = Jellyfin server_id;
                   album_id;
                   _;
                 } ) as item);
          |] ->
              let servers = Lwd_seq.to_list (Lwd.peek Servers.connexions) in
              let connexion : DS.connexion = List.assq server_id servers in
              let url = audio_url connexion id in
              let () = Console.log [ "Now playing:"; name; Jv.of_string url ] in
              let () =
                let open Brr_io.Media.Session in
                let session = of_navigator G.navigator in
                let cover =
                  get_album_cover_link ~base_url:connexion.base_url ~size:500
                    ~album_id ~cover_type:Front
                in
                let title = name in
                let album = "" in
                let artist = "" in
                Fut.await cover (fun img_src ->
                    let artwork =
                      [
                        {
                          Media_metadata.src = img_src;
                          sizes = "500x500";
                          type' = "image/jpeg";
                        };
                      ]
                    in
                    set_metadata session { title; artist; album; artwork })
              in
              { item; url }
          | _ -> raise Not_found
        in
        Lwd.set now_playing (Some item)

  let reset_playlist playlist =
    ignore @@ set_play_url (Some playlist) 0;
    Lwd.set playstate.playlist (Some playlist);
    Lwd.set playstate.current_index 0

  let make idb () =
    let audio_elt =
      El.audio
        ~at:
          [
            At.v (Jstr.v "controls") (Jstr.v "true");
            At.v (Jstr.v "autoplay") (Jstr.v "true");
            At.v (Jstr.v "preload") (Jstr.v "auto");
          ]
        []
    in
    let set_src url = El.set_at (Jstr.v "src") (Some (Jstr.v url)) audio_elt in
    let _auto_play =
      (* We cannot rely on the main [Lwd] observer for playback control because
         it is tied to the [requestAnimationFrames] callback. This prevent the
         player to start playing the next song if the tab is in the background.
      *)
      let root = Lwd.observe (Lwd.get now_playing) in
      Lwd.set_on_invalidate root (fun _ ->
          match Lwd.quick_sample root with
          | Some { url; _ } -> set_src url
          | None -> ());
      Lwd.quick_sample root |> ignore
    in
    let next () =
      let playlist = Lwd.peek playstate.playlist in
      let current_index = Lwd.peek playstate.current_index in
      let next_index = current_index + 1 in
      ignore @@ set_play_url playlist next_index;
      Lwd.set playstate.current_index next_index
    in
    let prev () =
      let playlist = Lwd.peek playstate.playlist in
      let current_index = Lwd.peek playstate.current_index in
      let next_index = max 0 (current_index - 1) in
      ignore @@ set_play_url playlist next_index;
      Lwd.set playstate.current_index next_index
    in
    let set_position_state =
      (* Enable control from OS *)
      let open Brr_io.Media.Session in
      let session = of_navigator G.navigator in
      let set_position_state () =
        let duration = El.prop (El.Prop.float (Jstr.v "duration")) audio_elt in
        if not (Float.is_nan duration) then
          let playback_rate =
            El.prop (El.Prop.float (Jstr.v "playbackRate")) audio_elt
          in
          let position =
            El.prop (El.Prop.float (Jstr.v "currentTime")) audio_elt
          in
          set_position_state ~duration ~playback_rate ~position session
      in
      set_action_handler session Action.next_track next;
      set_action_handler session Action.previous_track prev;
      set_position_state
    in
    let on_error ev =
      Ev.stop_immediate_propagation ev;
      Ev.prevent_default ev;
      Console.log
        [
          "A playback error happened. This is probably due to a codec \
           unsupported by the browser.";
          ev;
        ];
      next ()
    in
    let next _ = next () in
    let () =
      let target = El.as_target audio_elt in
      ignore @@ Ev.listen Ev.ended next target;
      ignore @@ Ev.listen Ev.error on_error target;
      ignore @@ Ev.listen Ev.play (fun _ -> set_position_state ()) target
    in
    let btn_next =
      Brr_lwd_ui.Button.v ~ev:[ `P (Elwd.handler Ev.click next) ] (`P "NEXT")
    in
    let open Brr_lwd_ui in
    let now_playing =
      let track_cover =
        let style =
          let cover =
            Lwd.map2 (Lwd.get now_playing) (Lwd.get Servers.connexions)
              ~f:(fun now_playing servers ->
                match now_playing with
                | None -> Lwd.pure "track.png"
                | Some
                    {
                      item = _, { album_id; server_id = Jellyfin server_id; _ };
                      _;
                    } ->
                    let servers = Lwd_seq.to_list servers in
                    let connexion : DS.connexion =
                      List.assq server_id servers
                    in
                    Lwd.get
                    @@ cover_var ~base_url:connexion.base_url ~size:500
                         ~album_id ~cover_type:Front)
          in
          Lwd.map (Lwd.join cover) ~f:(fun src ->
              Printf.sprintf "background-image: url(%S)" src)
        in
        let at =
          Attrs.(
            add At.Name.class' (`P "now-playing-cover") []
            |> add At.Name.style (`R style))
        in
        let on_click =
          Elwd.handler Ev.click (fun e ->
              Ev.prevent_default e;
              (match Lwd.peek App_state.active_layout with
              | Kiosk -> Main
              | Main -> Kiosk)
              |> Lwd.set App_state.active_layout;
              Lwd.set App_state.kiosk_cover Front)
        in
        Elwd.a
          ~ev:[ `P on_click ]
          ~at:[ `P (At.href (Jstr.v "#")) ]
          [ `R (Elwd.div ~at []) ]
      in
      let track_details =
        let at = Attrs.add At.Name.class' (`P "now-playing-details") [] in
        let default_album_title = "Unknown album" in
        let default_artist_name = "Unknown artist" in
        let album_title = Lwd.var default_album_title in
        let artist_name = Lwd.var default_artist_name in
        let update_album_title album_id =
          match album_id with
          | None -> Lwd.set album_title default_album_title
          | Some album_id ->
              let open Db.Stores in
              let album_index =
                IDB.Database.transaction
                  [ (module Albums_store) ]
                  ~mode:Readonly idb
                |> IDB.Transaction.object_store (module Albums_store)
                |> Albums_store.index (module Albums_by_idx) ~name:"by-idx"
              in
              let album =
                Albums_by_idx.get_key album_id album_index
                |> IDB.Request.fut_exn
              in
              Fut.await album
                (Option.iter
                   (fun { Db.Generic_schema.Album.Key.name; artists; _ } ->
                     Lwd.set album_title name;
                     let artist =
                       match artists with
                       | artist_id :: _ ->
                           let artist_store =
                             IDB.Database.transaction
                               [ (module Artists_store) ]
                               ~mode:Readonly idb
                             |> IDB.Transaction.object_store
                                  (module Artists_store)
                           in
                           Artists_store.get artist_id artist_store
                           |> IDB.Request.fut_exn
                       | _ -> Fut.return None
                     in
                     Fut.await artist (fun artist ->
                         Lwd.set artist_name
                         @@ Option.map_or ~default:default_artist_name
                              (fun { Db.Generic_schema.Artist.name; _ } -> name)
                              artist)))
        in
        let details =
          let txt =
            Lwd.map (Lwd.get now_playing) ~f:(function
              | None -> El.txt' "Nothing playing"
              | Some { item = { name; _ }, { album_id; _ }; _ } ->
                  update_album_title album_id;
                  El.txt' name)
          in
          let album_title =
            Lwd.map (Lwd.get album_title) ~f:(fun title -> El.txt' title)
          in
          let artist_txt =
            Lwd.map (Lwd.get artist_name) ~f:(fun title -> El.txt' title)
          in
          let on_click =
            Lwd.map (Lwd.get artist_name) ~f:(fun name ->
                Elwd.handler Ev.click (fun e ->
                    Ev.prevent_default e;
                    Lwd.set Ui_filters.artist_formula.value (Some ("+" ^ name))))
          in
          [
            `R Elwd.(div [ `R (span [ `R txt ]) ]);
            `R Elwd.(div [ `R (span [ `R album_title ]) ]);
            `R
              Elwd.(
                div
                  [
                    `R
                      (Elwd.a
                         ~ev:[ `R on_click ]
                         ~at:[ `P (At.href (Jstr.v "#")) ]
                         [ `R (span [ `R artist_txt ]) ]);
                  ]);
          ]
        in
        Elwd.div ~at details
      in
      let at =
        Attrs.(
          add At.Name.class' (`P "box") []
          |> add At.Name.class' (`P "now-playing-display"))
      in
      Elwd.div ~at [ `R track_cover; `R track_details ]
    in
    let at =
      Attrs.(
        add At.Name.class' (`P "player-wrapper") []
        |> add At.Name.class' (`P "box"))
    in
    Elwd.div ~at [ `R now_playing; `P audio_elt; `R btn_next ]
end

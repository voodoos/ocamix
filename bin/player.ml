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
  item :
    Db.Generic_schema.Track.Key.t
    * Db.Generic_schema.Track.t
    * Db.Generic_schema.Album.t option;
  url : string;
}

let now_playing = Lwd.var None

let idb =
  let idb, set_idb = Fut.create () in
  let _ = Db.with_idb @@ fun idb -> ignore (set_idb idb) in
  idb

let get_album_cover_link_opt ~base_url ~size album ~cover_type =
  (* Todo for better user experience we should pre-fetch the images before
     updating the DOM. This is especially true for back covers that come from
     the coverart archive which can be slow to download. *)
  let open Db.Generic_schema in
  match album with
  | None -> None
  | Some { Album.id = Id.Jellyfin id; mbid; _ } ->
      if Equal.poly cover_type App_state.Front then
        Some
          (Printf.sprintf "%s/Items/%s/Images/Primary?width=%i&format=Jpg"
             base_url id size)
      else
        Option.map
          (fun mbid ->
            Printf.sprintf "https://coverartarchive.org/release/%s/back-1200"
              mbid)
          mbid

let get_album_cover_link ~base_url ~size ~cover_type album =
  get_album_cover_link_opt ~base_url ~size ~cover_type album
  |> Option.value ~default:"track.png"

let get_stream_url connexion ~name item_id =
  let open Fut.Result_syntax in
  let+ stream =
    DS.audio_stream connexion
      ~device_profile:(Lazy.force Browser_profile.t)
      ~item_id
    |> Fut.map (Result.map_err (fun e -> `Jv e))
  in
  match stream with
  | None ->
      Console.error [ "The server has no playable source for the track"; name ];
      None
  | Some { DS.url; play_method; _ } ->
      Console.log
        [
          Format.asprintf "Now playing (%a):" DS.pp_play_method play_method;
          name;
          Jv.of_string url;
        ];
      Some url

module Playback_controller (P : sig
  val fetch :
    View.ranged ->
    int array ->
    ( (Db.Generic_schema.Track.Key.t
      * Db.Generic_schema.Track.t
      * Db.Generic_schema.Album.t option)
      option
      array,
      Db.Worker_api.error )
    Fut.result
end) =
struct
  type queued_track = { index : int; track : now_playing }

  let playback_request : (View.ranged * int) option Lwd.var = Lwd.var None

  let get_playable_track (playlist : View.ranged) index =
    (* [index] is relative to the selected track. [item_count] includes the
       skipped prefix, so use [View.item_count] to avoid indexing [order]
       past its final element when playback reaches the playlist end. *)
    if index < 0 || index >= View.item_count playlist.view then Fut.ok None
    else
      let open Db.Generic_schema in
      let open Fut.Result_syntax in
      let* result = P.fetch playlist [| index |] in
      match result with
      | [|
       Some
         Track.(
           ( { Key.name; _ },
             { id = Jellyfin id; server_id = Jellyfin server_id; _ },
             _album ) as item);
      |] ->
          let servers = Lwd_seq.to_list (Lwd.peek Servers.connexions) in
          let connexion : DS.connexion = List.assq server_id servers in
          let+ stream = get_stream_url connexion ~name id in
          Option.map (fun url -> { item; url }) stream
      | _ -> Fut.ok None

  let set_current_track track =
    let open Db.Generic_schema in
    let open Track in
    let { Key.name; _ }, { server_id = Jellyfin server_id; _ }, album =
      track.item
    in
    Lwd.set now_playing (Some track);
    let servers = Lwd_seq.to_list (Lwd.peek Servers.connexions) in
    let connexion : DS.connexion = List.assq server_id servers in
    let open Brr_io.Media.Session in
    let session = of_navigator G.navigator in
    let img_src =
      get_album_cover_link ~base_url:connexion.base_url ~size:500
        ~cover_type:Front album
    in
    let album = "" in
    let artist = "" in
    let artwork =
      [
        {
          Media_metadata.src = img_src;
          sizes = "500x500";
          type' = "image/jpeg";
        };
      ]
    in
    set_metadata session { title = name; artist; album; artwork }

  let reset_playlist playlist =
    Lwd.set playstate.playlist (Some playlist);
    Lwd.set playstate.current_index 0;
    Lwd.set playback_request (Some (playlist, 0))

  let make idb () =
    let prepared_tracks : queued_track Queue.t = Queue.create () in
    let next_track_index = ref 0 in
    let generation = ref 0 in
    let stream_ref : Audio_stream.t option ref = ref None in
    let rec fill_prepared_tracks (playlist : View.ranged) request_generation
        minimum on_ready =
      if
        request_generation = !generation
        && Queue.length prepared_tracks < minimum
        && !next_track_index < View.item_count playlist.view
      then begin
        let index = !next_track_index in
        incr next_track_index;
        Fut.await (get_playable_track playlist index) (function
          | Error _ ->
              fill_prepared_tracks playlist request_generation minimum on_ready
          | Ok None ->
              fill_prepared_tracks playlist request_generation minimum on_ready
          | Ok (Some track) ->
              if request_generation = !generation then begin
                Queue.add { index; track } prepared_tracks;
                Option.iter
                  (fun stream -> Audio_stream.queue_song stream track.url)
                  !stream_ref;
                fill_prepared_tracks playlist request_generation minimum
                  on_ready
              end)
      end
      else if request_generation = !generation then on_ready ()
    in
    let on_track_started () =
      match Queue.take_opt prepared_tracks with
      | None -> ()
      | Some { index; track } -> (
          set_current_track track;
          Lwd.set playstate.current_index index;
          match Lwd.peek playstate.playlist with
          | None -> ()
          | Some playlist ->
              fill_prepared_tracks playlist !generation 2 (fun () -> ()))
    in
    let audio_controls, stream =
      Audio_player.make_player ~on_track_change:on_track_started ()
    in
    stream_ref := Some stream;
    let load_playlist playlist index =
      incr generation;
      let request_generation = !generation in
      Audio_stream.reset stream;
      Queue.clear prepared_tracks;
      next_track_index := index;
      Lwd.set playstate.playlist (Some playlist);
      Lwd.set playstate.current_index index;
      (* The stream promotes the first queued track to [current] and keeps the
         following one ready for the crossfade. Queue one extra track so that
         there are always two upcoming tracks available. *)
      fill_prepared_tracks playlist request_generation 3 (fun () ->
          ignore @@ Audio_stream.resume stream)
    in
    let _playback_requests =
      (* Keep this observer independent from the main render loop: network
         requests and audio preloading must continue while the tab is hidden. *)
      let root = Lwd.observe (Lwd.get playback_request) in
      let load () =
        match Lwd.quick_sample root with
        | None -> ()
        | Some (playlist, index) -> load_playlist playlist index
      in
      Lwd.set_on_invalidate root (fun _ -> load ());
      load ();
      root
    in
    let next () = ignore @@ Audio_stream.force_next stream in
    let prev () =
      match Lwd.peek playstate.playlist with
      | None -> ()
      | Some playlist ->
          let current_index = Lwd.peek playstate.current_index in
          Lwd.set playback_request (Some (playlist, max 0 (current_index - 1)))
    in
    let _set_position_state =
      (* Enable control from OS *)
      let open Brr_io.Media.Session in
      let session = of_navigator G.navigator in
      let set_position_state () =
        Audio_stream.current_media_element stream
        |> Option.iter @@ fun media ->
           let duration = Brr_io.Media.El.duration_s media in
           if not (Float.is_nan duration) then
             let playback_rate = Brr_io.Media.El.playback_rate media in
             let position = Brr_io.Media.El.current_time_s media in
             set_position_state ~duration ~playback_rate ~position session
      in
      set_action_handler session Action.next_track next;
      set_action_handler session Action.previous_track prev;
      set_position_state
    in
    let next _ = next () in
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
                | None -> "track.png"
                | Some
                    {
                      item = _, { server_id = Jellyfin server_id; _ }, album;
                      _;
                    } ->
                    let servers = Lwd_seq.to_list servers in
                    let connexion : DS.connexion =
                      List.assq server_id servers
                    in
                    get_album_cover_link ~base_url:connexion.base_url ~size:500
                      ~cover_type:Front album)
          in
          Lwd.map cover ~f:(fun src ->
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
              let album_store =
                (* TODO we don't need to fetch it anymore *)
                IDB.Database.transaction
                  [ (module Albums_store) ]
                  ~mode:Readonly idb
                |> IDB.Transaction.object_store (module Albums_store)
              in
              let album =
                Albums_store.get album_id album_store |> IDB.Request.fut_exn
              in
              Fut.await album
                (Option.iter
                   (fun { Db.Generic_schema.Album.name; artists; _ } ->
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
              | Some { item = { name; _ }, { album_id; _ }, _; _ } ->
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
    Elwd.div ~at [ `R now_playing; `R audio_controls; `R btn_next ]
end

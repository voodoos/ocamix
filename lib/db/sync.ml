open! Std
open Brrer
open Brr
open Brr_io.Indexed_db
module Source = Data_source.Jellyfin
open Source.Api

(* Items Hierarchy

   UserRootFolder ("Media Folders" "e9d5075a555c1cbc394eec4cef295274")
                  (Gets the root folder from a user's library. )
   ^ CollectionFolder ("MusicLib" "150848cd4f44b9ae32ec5a7934de39ce")

   AggregateFolder ("root" "f27caa37e5142225cceded48f6553502")
   ^ Folder ("media" "92fde71d0ec577b531e7b3427b223bed")
   ^ Folder+ <- MusicArtist <- MusicAlbum <- Audio

   Jellyfin considers that Folder ("media" "92fde71d0ec577b531e7b3427b223bed") == CollectionFolder ("MusicLib" "150848cd4f44b9ae32ec5a7934de39ce")

   This equality can be retrived by querying the "VirtualFolders" and look at
   the locations' paths. Unfortunately this is not accessible to unpriviledged users...

   To actually know which folders are part of a user view we need to query the
   items that have this view's id as a [parentId]. The actual [parentId] of
   these items will be different and are the ids actual folders that
   constitute this view.

   For simplicity (?) we consider that an item is part of a view if its path is
   prefixed by one of the view's virtual folder locations.

   TODO: maybe we should use:
     "Gets all user media folders." /Library/MediaFolders
*)

type status =
  | Unknown
  | Syncing
  | In_sync
  | Inconsistent
  | New_items of {
      first_missing_key : int;
      first_unfetched_key : int;
      last_source_item_key : int;
    }
  | Partial_fetch of { first_unfetched_key : int; last_source_item_key : int }
[@@deriving jsont]

type count = {
  mutable artists : int;
  mutable albums : int;
  mutable tracks : int;
}
[@@deriving jsont]

let new_count () = { artists = 0; albums = 0; tracks = 0 }

type progress = { total : count; processed : count; jobs : int }
[@@deriving jsont]

type report = { status : status; sync_progress : progress option }
[@@deriving jsont]

let initial_report = { status = Unknown; sync_progress = None }

let status_to_string = function
  | Unknown -> "Unknown"
  | Syncing -> "Syncing"
  | In_sync -> "Synchronized"
  | Inconsistent -> "Inconsistent"
  | New_items { first_missing_key; first_unfetched_key; last_source_item_key }
    ->
      Format.sprintf "New items: last: %i missing: %i unfetched: %i"
        last_source_item_key first_missing_key first_unfetched_key
  | Partial_fetch { first_unfetched_key; last_source_item_key = _ } ->
      Format.sprintf "Partial: last: %i unfetched: %i" first_unfetched_key
        first_unfetched_key

let pp_progress fmt { total; processed; jobs } =
  Format.fprintf fmt ": %i/%i artists; %i/%i albums; %i/%i tracks [%i jobs]"
    processed.artists total.artists processed.albums total.albums
    processed.tracks total.tracks jobs

let pp_report fmt { status; sync_progress } =
  let status = status_to_string status in
  Format.fprintf fmt "%s%a" status
    (Format.pp_print_option pp_progress)
    sync_progress

let log_status = function
  | Unknown -> Console.info [ "Database status is unknown" ]
  | Syncing -> Console.info [ "Databae is being synchronized" ]
  | In_sync -> Console.info [ "Database is synchronized" ]
  | Inconsistent -> Console.warn [ "Database is out-of-sync" ]
  | New_items { first_missing_key; first_unfetched_key; last_source_item_key }
    ->
      Console.info
        [
          "New items were added to the source.";
          last_source_item_key - first_missing_key + 1;
          "new items";
          last_source_item_key - first_unfetched_key + 1;
          "unfetched items";
        ]
  | Partial_fetch { first_unfetched_key; last_source_item_key } ->
      Console.info
        [
          "Some items have not been fetched yet.";
          last_source_item_key - first_unfetched_key + 1;
          "unfetched items";
        ]

let update_collections source idb =
  let open Fut.Result_syntax in
  let* views =
    Source.query source
      (module Source.Api.Views)
      { include_external_content = false }
      { user_id = source.auth_response.user.id }
  in
  let init = Fut.ok [] in
  List.fold_left views.items ~init ~f:(fun acc (item : Item.t) ->
      let* acc = acc in
      let open Brr_io.Indexed_db in
      let transaction =
        Database.transaction
          [ (module Stores.Collections_store) ]
          ~mode:Readwrite idb
      in
      let s_collections =
        Transaction.object_store (module Stores.Collections_store) transaction
      in
      let collections_by_id = Stores.collections_by_id s_collections in
      let _sort_name = Option.value item.sort_name ~default:item.name in

      if String.equal "music" @@ Option.get_or ~default:"" item.collection_type
      then
        let open Fut.Result_syntax in
        let+ collection_id =
          let* idx =
            Stores.Collections_by_id.get_key (Jellyfin item.id)
              collections_by_id
            |> Request.fut
          in
          match idx with
          | Some idx -> Fut.ok idx
          | None ->
              Stores.Collections_store.add
                { id = Jellyfin item.id; name = item.name }
                s_collections
              |> Request.fut
        in

        (collection_id, item) :: acc
      else Fut.ok acc)

(* The synchronization is a flat traversal of the source: for each view we ask
   the server for every item it contains, with pagination. It used to be a
   recursive search issuing one query per folder, which turned into thousands of
   small round-trips on a large library.

   It runs in three phases, artists, then albums, then tracks, because
   [sync_track] resolves a track's album through the [Albums_by_id] index: the
   albums have to be in the database before the first track is stored. The
   recursive traversal used to guarantee that by visiting parents first. *)

let get_music_brainz_id external_urls =
  List.find_map external_urls ~f:(fun { Source.Api.Item.name; url } ->
      if String.equal_caseless "MusicBrainz" name then
        (* https://musicbrainz.org/artist/d2e06763-1035-4b1a-82c7-b7c08e06ba48 *)
        String.split_on_char ~by:'/' url |> List.last_opt
      else None)

let store_artist store { Item.type_; name; id; external_urls; _ } =
  if not (Equal.poly type_ MusicArtist) then (
    Console.error [ "store_artist: not an artist! "; id ];
    Fut.error (Jv.Error.v (Jstr.v "Wrong argument: not an artist")))
  else
    let mbid = get_music_brainz_id external_urls in
    let canon = canonicalize_string name in
    (* TODO There is no sort name in jellyfin's db... *)
    let sort_name = "" in
    Stores.Artists_store.add
      { id = Jellyfin id; mbid; name; canon; sort_name }
      store
    |> Request.on_error ~f:(fun e _ -> Ev.prevent_default e)
    |> Request.fut

let find_artist_idx idb id =
  let open Fut.Syntax in
  let transaction =
    Database.transaction [ (module Stores.Artists_store) ] ~mode:Readonly idb
  in
  let index =
    Transaction.object_store (module Stores.Artists_store) transaction
    |> Stores.Artists_store.index (module Stores.Artists_by_id) ~name:"by-id"
  in
  let+ result =
    Stores.Artists_by_id.get_key (Generic_schema.Id.Jellyfin id) index
    |> Request.fut
  in
  match result with Ok idx -> idx | Error _ -> None

(* Lookup artists from the db. They should all be present after the "Artists"
   phase *)
let find_artists_idx _source idb artist_items =
  let open Fut.Syntax in
  let+ all =
    List.map
      ~f:(fun ({ id; _ } : Item.artist_item) -> find_artist_idx idb id)
      artist_items
    |> Fut.of_list
  in
  List.filter_map ~f:Fun.id all

let sync_artists ~source:_ idb items : (unit, Jv.Error.t) Fut.result =
  let open Brr_io.Indexed_db in
  let transaction =
    Database.transaction [ (module Stores.Artists_store) ] ~mode:Readwrite idb
  in
  let store =
    Transaction.object_store (module Stores.Artists_store) transaction
  in
  List.fold_left items ~init:(Fut.ok ()) ~f:(fun acc -> function
    | { Item.type_ = MusicArtist; name; _ } as artist ->
        let open Fut.Syntax in
        let* result = store_artist store artist in
        Result.iter_err
          (fun error ->
            (* This happens when the item is already in the database *)
            (* TODO: It would be cleaner to check for dups before inserting.
               Especially since none of the current indexes clearly states what's a
               dup [<> musicbrainz id || <> jellyfin id] *)
            Console.warn [ "Could not add artist into the db: "; name ];
            Console.warn [ Jv.Error.message error ])
          result;
        acc
    | _ -> failwith "Not an artist")

let genres_memo : (string, (int, Jv.Error.t) Fut.result) Hashtbl.t =
  Hashtbl.create 256

let prepare_genres idb genre_items =
  let get_or_set_genre (name, canon) =
    match Hashtbl.get genres_memo canon with
    | Some key -> key
    | None ->
        let transaction =
          Database.transaction
            [ (module Stores.Genres_store) ]
            ~mode:Readwrite idb
        in
        let s_genres =
          Transaction.object_store (module Stores.Genres_store) transaction
        in
        let i_genres =
          Stores.Genres_store.index
            (module Stores.Genres_by_canonical_name)
            ~name:"genres_by_canon_name" s_genres
        in
        let key =
          Stores.Genres_by_canonical_name.get_key canon i_genres
          |> Request.fut_exn
          |> Fun.flip Fut.bind (function
            | Some key -> Fut.ok key
            | None ->
                let genre = Generic_schema.{ Genre.name; canon } in
                Stores.Genres_store.add genre s_genres
                (* Without this a duplicate would abort the whole transaction,
                   and with it the album or track being synchronized. *)
                |> Request.on_error ~f:(fun e _ -> Ev.prevent_default e)
                |> Request.fut)
        in
        (* Memoized before any await: see [genres_memo]. *)
        Hashtbl.add genres_memo canon key;
        key
  in
  List.concat_map genre_items
    ~f:(fun ({ name; _ } : Source.Api.Item.genre_item) ->
      String.split_on_char ~by:';' name
      |> List.concat_map ~f:(String.split_on_char ~by:',')
      |> List.map ~f:(fun name ->
          let name = String.trim name in
          (name, canonicalize_string name))
      |> List.uniq ~eq:(fun (_, c1) (_, c2) -> String.equal c1 c2)
      |> List.map ~f:get_or_set_genre)
  |> Fut.of_list |> Fut.map Result.flatten_l

let sync_albums ~source idb items : (unit, Jv.Error.t) Fut.result =
  let open Brr_io.Indexed_db in
  let sync_album
      {
        Source.Api.Item.name;
        id;
        date_created;
        external_urls;
        sort_name;
        genre_items;
        album_artists = artist_items;
        image_tags;
        image_blur_hashes;
        run_time_ticks = duration;
        _;
      } =
    let open Fut.Syntax in
    let* artists = find_artists_idx source idb artist_items in
    (* TODO use Musicbrainz ids for dedup *)
    let mbid = get_music_brainz_id external_urls in
    let sort_name = Option.value ~default:name sort_name in
    (* TODO There is no sort name in jellyfin's db... *)
    let open Fut.Result_syntax in
    let* genres = prepare_genres idb genre_items in
    let transaction =
      Database.transaction [ (module Stores.Albums_store) ] ~mode:Readwrite idb
    in
    let store =
      Transaction.object_store (module Stores.Albums_store) transaction
    in
    let id = Generic_schema.Id.Jellyfin id in
    let blur_hashes =
      String.Map.filter_map
        (fun kind (* ex: Primary *) hashes ->
          let open Option in
          let* tag = String.Map.find_opt kind image_tags in
          String.Map.find_opt tag hashes)
        image_blur_hashes
    in
    let date_created =
      Option.get_exn_or "Album should have an creation date" date_created
    in
    Stores.Albums_store.add
      {
        id;
        date_created;
        mbid;
        name;
        sort_name;
        genres;
        artists;
        duration;
        blur_hashes;
      }
      store
    |> Request.on_error ~f:(fun e _ -> Ev.prevent_default e)
    |> Request.fut
  in
  List.fold_left items ~init:(Fut.ok ()) ~f:(fun acc item ->
      match item with
      | { Item.type_ = MusicAlbum; name; _ } as album -> (
          let open Fut.Syntax in
          let* result = sync_album album in
          match result with
          | Error error ->
              (* This happens when the item is already in the database *)
              Console.warn [ "Could not add album into the db: "; name ];
              Console.warn [ Jv.Error.message error ];
              acc
          | Ok _ -> acc)
      | _ -> failwith "Not an album")

let count_tracks = ref 0

let sync_tracks ~collection_id ~source idb items : (unit, Jv.Error.t) Fut.result
    =
  let open Brr_io.Indexed_db in
  let sync_track
      {
        Source.Api.Item.name;
        id;
        date_created;
        sort_name;
        genre_items;
        artist_items;
        album_artists;
        server_id;
        album_id;
        parent_index_number;
        index_number;
        run_time_ticks;
        _;
      } =
    let () = incr count_tracks in
    let open Fut.Syntax in
    let* artists = find_artists_idx source idb artist_items
    and* album_artists = find_artists_idx source idb album_artists in
    (* TODO Artists *)
    let sort_name = Option.value ~default:name sort_name in
    (* TODO There is no sort name in jellyfin's db... *)
    let open Fut.Result_syntax in
    let* genres = prepare_genres idb genre_items in
    let transaction =
      Database.transaction
        [ (module Stores.Tracks_store); (module Stores.Albums_store) ]
        ~mode:Readwrite idb
    in
    let store =
      Transaction.object_store (module Stores.Tracks_store) transaction
    in
    let albums_store =
      Transaction.object_store (module Stores.Albums_store) transaction
    in
    let id = Generic_schema.Id.Jellyfin id in
    let date_created =
      Option.get_exn_or "Track should have an creation date" date_created
    in
    let key =
      (* TODO: can an item be part of multiple collections ? *)
      {
        Generic_schema.Track.Key.id;
        name;
        date_created;
        genres;
        artists;
        album_artists;
        collections = [ collection_id ];
        duration = run_time_ticks;
      }
    in
    let+ album_id =
      match album_id with
      | Some id -> (
          let albums_by_id =
            Stores.Albums_store.index
              (module Stores.Albums_by_id)
              ~name:"by-id" albums_store
          in
          let+ result =
            Stores.Albums_by_id.get_key (Generic_schema.Id.Jellyfin id)
              albums_by_id
            |> Request.fut
          in
          match result with None -> None | Some idx -> Some idx)
      | None -> Fut.ok None
    in
    let track_index = Option.get_or ~default:1 index_number in
    Stores.Tracks_store.add ~key
      {
        id;
        server_id = Jellyfin server_id;
        album_id;
        sort_name;
        disc_index = parent_index_number;
        track_index;
      }
      store
    |> Request.on_error ~f:(fun e _ ->
        (* This happens when the item is already in the database *)
        Console.warn [ "Could not add album into the db: "; name ];
        Ev.prevent_default e)
    |> Request.fut
  in
  List.fold_left items ~init:(Fut.ok ()) ~f:(fun acc item ->
      match item with
      | { Item.type_ = Audio; name; _ } as track ->
          let open Fut.Syntax in
          let* result = sync_track track in
          Result.iter_err
            (fun error ->
              (* This happens when the item is already in the database *)
              Console.warn [ "Could not add track into the db: "; name ];
              Console.warn [ Jv.Error.message error ])
            result;
          acc
      | _ -> failwith "Not a track")

(* A sync phase. The order matters: [sync_track] resolves a track's album
   through the [Albums_by_id] index, so every album must have been stored
   before the first track is. *)
type phase = Artists | Albums | Tracks

type job = {
  collection_id : int;
  view_id : string;
  phase : phase;
  start_index : int;
  count : int;  (** used to report progress *)
}

let string_of_phase = function
  | Artists -> "artists"
  | Albums -> "albums"
  | Tracks -> "tracks"

let item_types_of_phase = function
  | Artists -> [ Item.MusicArtist ]
  | Albums -> [ Item.MusicAlbum ]
  | Tracks -> [ Item.Audio ]

let fields_of_phase = function
  | Artists -> [ Item.ExternalUrls ] (* mbid *)
  | Albums -> [ Item.Genres; DateCreated; ExternalUrls ]
  | Tracks -> [ Item.Genres; DateCreated ]

let enable_images_of_phase = function
  | Albums -> true
  | Artists | Tracks -> false

let pool_size = 10
let page_size = function Artists -> 500 | Albums -> 250 | Tracks -> 1000

let count_items ~source ~parent_id ~types =
  let open Fut.Result_syntax in
  let open Source in
  let req =
    Api.Items.
      {
        ids = [];
        parent_id = Some parent_id;
        user_id = source.auth_response.user.id;
        fields = [];
        include_item_types = types;
        start_index = None;
        limit = Some 0;
        sort_by = [];
        sort_order = None;
        recursive = true;
        enable_user_data = false;
        enable_images = false;
        enable_total_record_count = true;
      }
  in
  let+ result = query source (module Api.Items) req () in
  result.total_record_count

(* Fetch one page and store what it contains. The three [sync_*] functions
   filter on the item type and pass the rest through, so they are no-ops on
   items of the other phases. *)
let sync_page ~source ~idb { collection_id; view_id; phase; start_index; count }
    =
  let open Fut.Result_syntax in
  let open Source in
  let req =
    Api.Items.
      {
        ids = [];
        parent_id = Some view_id;
        user_id = source.auth_response.user.id;
        fields = fields_of_phase phase;
        include_item_types = item_types_of_phase phase;
        start_index = Some start_index;
        limit = Some count;
        sort_by = [];
        sort_order = None;
        recursive = true;
        enable_user_data = false;
        enable_images = enable_images_of_phase phase;
        enable_total_record_count = false;
      }
  in
  let* { Api.Items.items; _ } = query source (module Api.Items) req () in
  match phase with
  | Artists -> sync_artists ~source idb items
  | Albums -> sync_albums ~source idb items
  | Tracks -> sync_tracks ~collection_id ~source idb items

let pool_iter ~parallelism ?(on_start = fun _ -> ()) ?(on_done = fun _ _ -> ())
    ~f jobs =
  let jobs = Array.of_list jobs in
  let n = Array.length jobs in
  let next = ref 0 in
  let failures = ref 0 in
  let rec worker () =
    let i = !next in
    if i >= n then Fut.return ()
    else begin
      (* No await between the read and the write above: nothing else can be
         handed this index. *)
      incr next;
      let job = jobs.(i) in
      on_start job;
      let open Fut.Syntax in
      let* result = f job in
      let () =
        match result with
        | Error err ->
            incr failures;
            Console.warn [ "Sync job failed: "; err ]
        | Ok () -> ()
      in
      on_done job result;
      (worker [@tailcall]) ()
    end
  in
  let open Fut.Syntax in
  let+ _ =
    List.init (min parallelism n) ~f:(fun _ -> worker ()) |> Fut.of_list
  in
  !failures

let pages_of_count ~collection_id ~view_id ~phase total =
  let size = page_size phase in
  List.init
    ((total + size - 1) / size)
    ~f:(fun i ->
      let start_index = i * size in
      {
        collection_id;
        view_id;
        phase;
        start_index;
        count = min size (total - start_index);
      })

let get_db_track_count idb ~collection_id =
  let open Fut.Result_syntax in
  let transaction =
    Database.transaction [ (module Stores.Tracks_store) ] ~mode:Readonly idb
  in
  let store =
    Transaction.object_store (module Stores.Tracks_store) transaction
  in
  let+ all_tracks = Stores.Tracks_store.get_all_keys store |> Request.fut in
  Array.fold_left all_tracks ~init:0
    ~f:(fun acc { Generic_schema.Track.Key.collections; _ } ->
      if List.exists ~f:(Int.equal collection_id) collections then acc + 1
      else acc)

let count_job count job =
  match job.phase with
  | Artists -> count.artists <- job.count + count.artists
  | Albums -> count.albums <- job.count + count.albums
  | Tracks -> count.tracks <- job.count + count.tracks

let sync_v2 ~report ~(source : Source.connexion) idb =
  let open Fut.Result_syntax in
  Console.info [ "Syncing database" ];
  let* views = update_collections source idb in
  let* jobs =
    List.map views ~f:(fun (collection_id, (view : Item.t)) ->
        let view_id = view.id in
        let* src_track_count =
          count_items ~source ~parent_id:view_id ~types:[ Audio ]
        in
        let* db_track_count = get_db_track_count idb ~collection_id in
        let () =
          Console.log
            [
              "Collection ";
              view.name;
              ": ";
              src_track_count;
              " tracks (";
              db_track_count;
              " in db)";
            ]
        in
        if src_track_count <= db_track_count then Fut.ok []
        else
          let* artist_count =
            count_items ~source ~parent_id:view_id ~types:[ MusicArtist ]
          in
          let+ album_count =
            count_items ~source ~parent_id:view_id ~types:[ MusicAlbum ]
          in
          let () =
            Console.log
              [ "  -> "; artist_count; " artists, "; album_count; " albums" ]
          in
          (* Artists only appear here when they exist as actual folders in the
             library. When they don't, [find_artists_idx] resolves them by id
             instead and this phase is simply empty. *)
          if Int.equal 0 album_count then
            Console.warn
              [
                "No album is reachable from view ";
                view.name;
                ": its tracks will have no album.";
              ];
          List.concat
            [
              pages_of_count ~collection_id ~view_id ~phase:Artists artist_count;
              pages_of_count ~collection_id ~view_id ~phase:Albums album_count;
              pages_of_count ~collection_id ~view_id ~phase:Tracks
                src_track_count;
            ])
    |> Fut.of_list |> Fut.map Result.flatten_l
    |> Fut.map (Result.map List.concat)
  in
  let total = new_count () in
  let () = List.iter jobs ~f:(count_job total) in
  let processed = new_count () in
  let running_jobs = ref 0 in
  let report_progress () =
    if total.artists + total.albums + total.tracks > 0 then
      report (Some { total; processed; jobs = !running_jobs })
  in
  let run_phase phase =
    let phase_jobs =
      List.filter jobs ~f:(fun job -> Equal.poly job.phase phase)
    in
    let open Fut.Syntax in
    let+ failures =
      pool_iter ~parallelism:pool_size
        ~on_start:(fun _ ->
          incr running_jobs;
          report_progress ())
        ~on_done:(fun job _ ->
          decr running_jobs;
          count_job processed job;
          report_progress ())
        ~f:(sync_page ~source ~idb) phase_jobs
    in
    if failures > 0 then
      Console.warn
        [ "Sync: "; failures; " "; string_of_phase phase; " pages failed" ];
    Ok ()
  in
  let* () = run_phase Artists in
  let* () = run_phase Albums in
  let+ () = run_phase Tracks in
  Hashtbl.reset genres_memo;
  Console.log [ "Sync finished. Added "; !count_tracks; " tracks" ]

let check_and_sync ?(report = fun _ -> ()) ~source idb =
  let open Fut.Result_syntax in
  let initial = initial_report in
  let () = (* Send a first report *) report initial in
  let sync_report_throttler = Limiter.throttle ~delay_ms:250 in
  let report' =
   fun sync_progress ->
    sync_report_throttler (fun () -> report { status = Syncing; sync_progress })
  in
  let+ () = sync_v2 ~report:report' ~source idb in
  sync_report_throttler (fun () ->
      report { status = In_sync; sync_progress = None })

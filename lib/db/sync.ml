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

type progress = { total : int; remaining : int; jobs : int }
type report = { status : status; sync_progress : progress option }

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

let pp_progress fmt { total; remaining; jobs } =
  Format.fprintf fmt "(%i/%i) [%i jobs]" remaining total jobs

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

(* The new sync process is not based on the full list of items anymore since
   this is too slow. It's a classic recursive search. The queue is initially
   populated with the top-level views and each time new folders are found they are
   added to the queue. The synchronization process ends when the queue is empty.

   Additionnal queries to find missing artists are performed on demand by the
   [find_artists_idx] function *)

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
    if String.equal name "ABBA" then Console.error [ "FOUND "; name; id ];
    (* TODO There is no sort name in jellyfin's db... *)
    let sort_name = "" in
    Stores.Artists_store.add
      { id = Jellyfin id; mbid; name; canon; sort_name }
      store
    |> Request.on_error ~f:(fun e _ -> Ev.prevent_default e)
    |> Request.fut

(* Todo: that's okay for the prototype, but there are alternative to these global
   tables:
      - Thread a map through the folds
      - Query the DB itself *)
let artists_ids : (string, int option Fut.t) Hashtbl.t = Hashtbl.create 128
let genres_memo = Hashtbl.create 256

(* Only album arstis are accessible via the recursive traversal. Other artists
   items have no parent and must be fetch specifically. *)
let find_artists_idx source idb artist_items =
  (* Some artists might already have been queried for, others not *)
  let ready, todo =
    List.partition_map_either artist_items
      ~f:(fun ({ id; _ } : Item.artist_item) ->
        match Hashtbl.get artists_ids id with
        | None -> Right id
        | Some fut -> Left fut)
  in
  let open Fut.Syntax in
  let newly_queried =
    let todo_n = List.length todo in
    if Int.equal 0 todo_n then []
    else
      let open Source in
      let user_id = source.auth_response.user.id in
      let now_futures : (string, int option -> unit) Hashtbl.t =
        Hashtbl.create todo_n
      in
      let futs =
        List.map todo ~f:(fun id ->
            let fut, set = Fut.create () in
            let set v =
              (* We remove elements when set to track stalled ones *)
              Hashtbl.remove now_futures id;
              set v
            in
            Hashtbl.add artists_ids id fut;
            Hashtbl.add now_futures id set;
            fut)
      in
      let params =
        {
          Api.Items.ids = todo;
          parent_id = None;
          user_id;
          fields = [];
          include_item_types = [ MusicArtist ];
          start_index = None;
          limit = None;
          sort_order = None;
          sort_by = [];
          recursive = false;
          enable_user_data = false;
          enable_images = false;
        }
      in
      Fut.await
        (query source (module Api.Items) params ())
        (function
          | Ok { items; _ } ->
              let transaction =
                Database.transaction
                  [ (module Stores.Artists_store) ]
                  ~mode:Readwrite idb
              in
              let store =
                Transaction.object_store
                  (module Stores.Artists_store)
                  transaction
              in
              let store_operation =
                List.map items ~f:(fun (artist : Item.t) ->
                    let set = Hashtbl.find now_futures artist.id in
                    store_artist store artist
                    |> Fut.map @@ function
                       | Error err ->
                           Console.warn [ "Store artist:"; artist.name; err ];
                           set None
                       | Ok idx -> set (Some idx))
                |> Fut.of_list
              in
              (* We invalidate remaining futures *)
              Fut.await store_operation (fun _ ->
                  Hashtbl.iter (fun _ set -> set None) now_futures)
          | Error err ->
              Console.error [ "Artists fetch failed: "; err ];
              Hashtbl.iter (fun _ set -> set None) now_futures);
      futs
  in
  let+ all = Fut.of_list (List.rev_append ready newly_queried) in
  List.filter_map ~f:Fun.id all

let sync_artists ~source:_ idb items : (Item.t list, Jv.Error.t) Fut.result =
  let open Fut.Result_syntax in
  let open Brr_io.Indexed_db in
  let transaction =
    Database.transaction [ (module Stores.Artists_store) ] ~mode:Readwrite idb
  in
  let store =
    Transaction.object_store (module Stores.Artists_store) transaction
  in
  List.fold_left items ~init:(Fut.ok []) ~f:(fun acc -> function
    | { Item.type_ = MusicArtist; name; id; _ } as artist -> (
        let open Fut.Syntax in
        let* result = store_artist store artist in
        match result with
        | Error error ->
            (* This happens when the item is already in the database *)
            (* TODO: It would be cleaner to check for dups before inserting.
               Especially since none of the current indexes clearly states what's a
               dup [<> musicbrainz id || <> jellyfin id] *)
            Console.warn [ "Could not add artist into the db: "; name ];
            Console.warn [ Jv.Error.message error ];
            acc
        | Ok idx ->
            Hashtbl.add artists_ids id (Fut.return (Some idx));
            acc)
    | item ->
        let+ acc = acc in
        item :: acc)

let prepare_genres idb genre_items =
  let get_or_set_genre (name, canon) =
    let open Fut.Result_syntax in
    match Hashtbl.get genres_memo canon with
    | Some key -> Fut.ok key
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
        let+ key =
          Stores.Genres_by_canonical_name.get_key canon i_genres
          |> Request.fut_exn
          |> Fun.flip Fut.bind (function
               | Some key -> Fut.ok key
               | None ->
                   let genre = Generic_schema.{ Genre.name; canon } in
                   Stores.Genres_store.add genre s_genres |> Request.fut)
        in
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

let sync_albums ~source idb items : (Item.t list, Jv.Error.t) Fut.result =
  let open Fut.Result_syntax in
  let open Brr_io.Indexed_db in
  let sync_album
      {
        Source.Api.Item.name;
        id;
        external_urls;
        sort_name;
        genre_items;
        album_artists = artist_items;
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
    let* idx =
      let albums_by_idx =
        Stores.Albums_store.index
          (module Stores.Albums_by_idx)
          ~name:"by-idx" store
      in
      (* We get the last id for the manual auto-increent field *)
      let+ result =
        Stores.Albums_by_idx.open_key_cursor ~direction:Prev albums_by_idx
        |> Request.fut
      in
      match result with
      | None -> 0
      | Some cursor ->
          (Stores.Albums_by_idx.Cursor.key cursor |> Option.get_or ~default:(-1))
          + 1
    in
    let id = Generic_schema.Id.Jellyfin id in
    Stores.Albums_store.add
      ~key:{ id; name; genres; artists }
      { idx; id; mbid; sort_name }
      store
    |> Request.on_error ~f:(fun e _ -> Ev.prevent_default e)
    |> Request.fut
  in
  List.fold_left items ~init:(Fut.ok []) ~f:(fun acc item ->
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
      | item ->
          let+ acc = acc in
          item :: acc)

let count_tracks = ref 0

let sync_tracks ~collection_id ~source idb items :
    (Item.t list, Jv.Error.t) Fut.result =
  let open Fut.Result_syntax in
  let open Brr_io.Indexed_db in
  let sync_track
      {
        Source.Api.Item.name;
        id;
        sort_name;
        genre_items;
        artist_items;
        album_artists;
        server_id;
        album_id;
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
    let key =
      (* TODO: can an item be part of multiple collections ? *)
      {
        Generic_schema.Track.Key.id;
        name;
        genres;
        artists;
        album_artists;
        collections = [ collection_id ];
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
            Stores.Albums_by_id.get (Generic_schema.Id.Jellyfin id) albums_by_id
            |> Request.fut
          in
          match result with None -> None | Some { idx; _ } -> Some idx)
      | None -> Fut.ok None
    in
    Stores.Tracks_store.add ~key
      { id; server_id = Jellyfin server_id; album_id; sort_name }
      store
    |> Request.on_error ~f:(fun e _ ->
           (* This happens when the item is already in the database *)
           Console.warn [ "Could not add album into the db: "; name ];
           Ev.prevent_default e)
    |> Request.fut
  in
  List.fold_left items ~init:(Fut.ok []) ~f:(fun acc item ->
      match item with
      | { Item.type_ = Audio; name; _ } as track -> (
          let open Fut.Syntax in
          let* result = sync_track track in
          match result with
          | Error error ->
              (* This happens when the item is already in the database *)
              Console.warn [ "Could not add track into the db: "; name ];
              Console.warn [ Jv.Error.message error ];
              acc
          | Ok _ -> acc)
      | item ->
          let+ acc = acc in
          item :: acc)

let sync_folder ~source ~collection_id ~(folder : Item.t) idb =
  let open Fut.Result_syntax in
  let open Source in
  (* We delegate recursive search to the server once we reached the artist's level *)
  let recursive = Equal.poly folder.type_ Item.MusicArtist in
  let req =
    Api.Items.
      {
        ids = [];
        parent_id = Some folder.id;
        user_id = source.auth_response.user.id;
        fields = [ ParentId; Path; Genres; DateCreated; ExternalUrls; Tags ];
        include_item_types = [ Source.Api.Item.MusicArtist; MusicAlbum; Audio ];
        start_index = None;
        limit = None;
        sort_by = [ DateCreated ];
        sort_order = Some Ascending;
        recursive;
        enable_user_data = false;
        enable_images = true;
      }
  in
  let* { Api.Items.start_index = _; items; _ } =
    query source (module Api.Items) req ()
  in
  let* remaining = sync_artists ~source idb items in
  let* remaining = sync_albums ~source idb remaining in
  let+ _remaining = sync_tracks ~collection_id ~source idb remaining in
  if recursive then []
  else
    List.fold_left items ~init:[] ~f:(fun acc -> function
      | { Item.is_folder = true; _ } as item -> (collection_id, item) :: acc
      | _ -> acc)

let get_source_track_count source view =
  let open Fut.Result_syntax in
  let open Source in
  let req =
    Api.Items.
      {
        ids = [];
        parent_id = Some view.Item.id;
        user_id = source.auth_response.user.id;
        fields = [];
        include_item_types = [ Audio ];
        start_index = None;
        limit = Some 0;
        sort_by = [];
        sort_order = None;
        recursive = true;
        enable_user_data = false;
        enable_images = false;
      }
  in
  let+ result = query source (module Api.Items) req () in
  result.total_record_count

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

let sync_v2 ~report ~(source : Source.connexion) idb =
  let open Fut.Result_syntax in
  Console.info [ "Syncing database" ];
  let* views = update_collections source idb in
  let queue = Queue.create () in
  let workers : int Fut.t Queue.t = Queue.create () in
  let* _ =
    List.map views ~f:(fun (collection_id, view) ->
        let* src_track_count = get_source_track_count source view in
        let+ db_track_count = get_db_track_count idb ~collection_id in
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
        if src_track_count > db_track_count then
          Queue.add (collection_id, view) queue)
    |> Fut.of_list |> Fut.map Result.flatten_l
  in
  let max_queue_length = ref (Queue.length queue) in
  let add_to_queue v =
    max_queue_length := !max_queue_length + 1;
    Queue.add v queue
  in
  let running_jobs = ref 0 in
  let run_job ~worker ~worker_is_ready (collection_id, folder) =
    incr running_jobs;
    let+ children = sync_folder ~source ~collection_id ~folder idb in
    List.iter children ~f:add_to_queue;
    decr running_jobs;
    worker_is_ready worker
  in
  let sync_all ~threads =
    let () =
      for i = 1 to threads do
        Queue.add (Fut.return i) workers
      done
    in
    let renaming () = Queue.length queue + !running_jobs in
    let rec assign_work acc () =
      (* Wait for a worker *)
      let next_worker = Queue.take_opt workers in
      match next_worker with
      | None -> acc
      | Some next_worker -> (
          let future_worker, worker_is_ready = Fut.create () in
          let () = Queue.add future_worker workers in
          Fut.bind next_worker @@ fun worker ->
          match Queue.take_opt queue with
          | None ->
              worker_is_ready worker;
              acc
          | Some job ->
              let () =
                report
                @@ Some
                     {
                       total = !max_queue_length;
                       remaining = renaming ();
                       jobs = !running_jobs;
                     }
              in
              let worker =
                let* () = run_job ~worker ~worker_is_ready job in
                (assign_work (Fut.ok ()) [@tailcall]) ()
              in
              (assign_work (Fut.bind acc (fun _ -> worker)) [@tailcall]) ())
    in
    assign_work (Fut.ok ()) ()
  in
  let+ () = sync_all ~threads:50 in
  Hashtbl.reset genres_memo;
  Hashtbl.reset artists_ids;
  Console.log [ "Sync finished. Added "; !count_tracks; " tracks" ]

let throttle ~delay_ms =
  (* We use [last_update] to have regular debounced updates and the
     [timeout] to ensure that the last event is always taken into
     account even it it happens during the debouncing interval. *)
  let last_update = ref 0. in
  let timer = ref (-1) in
  fun f ->
    let now = Performance.now_ms G.performance in
    if !timer >= 0 then G.stop_timer !timer;
    if now -. !last_update >. float_of_int delay_ms then (
      last_update := now;
      f ())
    else timer := G.set_timeout ~ms:delay_ms f

let check_and_sync ?(report = fun _ -> ()) ~source idb =
  let open Fut.Result_syntax in
  let initial = initial_report in
  let () = (* Send a first report *) report initial in
  let sync_report_throttler = throttle ~delay_ms:250 in
  let report' =
   fun sync_progress ->
    sync_report_throttler (fun () -> report { status = Syncing; sync_progress })
  in
  let+ () = sync_v2 ~report:report' ~source idb in
  sync_report_throttler (fun () ->
      report { status = In_sync; sync_progress = None })

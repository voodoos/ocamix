open! Std
open Brrer
open Brr
open Brr_io.Indexed_db
module Source = Data_source.Jellyfin
module OI = Stores.Orderred_items_store
module I = Stores.Items_store
module VF = Stores.Virtual_folder_store
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
*)

let chunk_size = 2000

let include_item_types =
  [ Source.Api.Item.MusicArtist; MusicAlbum; Audio; MusicGenre; Genre ]

let fetch_total_item_count source =
  let open Fut.Result_syntax in
  let+ res =
    Source.query source
      (module Source.Api.Items)
      Source.Api.Items.
        {
          (* todo make sort explicit (by date added date)*)
          ids = [];
          parent_id = None;
          user_id = source.auth_response.user.id;
          fields = [];
          include_item_types;
          start_index = None;
          limit = Some 0;
          sort_order = Some Ascending;
          sort_by = [ DateCreated ];
          recursive = true;
          enable_user_data = false;
          enable_images = false;
        }
      ()
  in
  res.total_record_count

type db_infos = {
  last_key : int option;
  last_value : Stores.Orderred_items.t option;
}

(* TODO this is wrong with the new DB schema. *)
let get_db_infos idb =
  let infos, set_infos = Fut.create () in
  let transaction = Database.transaction [ (module OI) ] ~mode:Readonly idb in
  let store = Transaction.object_store (module OI) transaction in
  let req = OI.open_cursor ~direction:Prev store in
  let last_key = ref None in
  let _ =
    Request.on_success req ~f:(fun _ q ->
        match Request.result q with
        | None -> set_infos { last_key = !last_key; last_value = None }
        | Some cursor -> (
            if Option.is_none !last_key then
              last_key := OI.Cursor_with_value.key cursor;
            match OI.Cursor_with_value.value cursor with
            | Some ({ item = Some _; _ } as item) ->
                set_infos { last_key = !last_key; last_value = Some item }
            | _ -> OI.Cursor_with_value.continue cursor))
  in
  infos

let is_db_consistent ~source:_ ~last_source_item_key db_sync_infos =
  let last_key = Option.value ~default:(-1) db_sync_infos.last_key in
  if last_key > last_source_item_key then
    (* There are fewer items in the source than expected *)
    false
  else
    (* Todo: check that the last known item is the same than in the source *)
    true

type status =
  | Unknown
  | In_sync
  | Inconsistent
  | New_items of {
      first_missing_key : int;
      first_unfetched_key : int;
      last_source_item_key : int;
    }
  | Partial_fetch of { first_unfetched_key : int; last_source_item_key : int }

type progress = { total : int; remaining : int }
type report = { status : status; sync_progress : progress option }

let initial_report = { status = Unknown; sync_progress = None }

let status_to_string = function
  | Unknown -> "Unknown"
  | In_sync -> "Synchronized"
  | Inconsistent -> "Inconsistent"
  | New_items { first_missing_key; first_unfetched_key; last_source_item_key }
    ->
      Format.sprintf "New items: last: %i missing: %i unfetched: %i"
        last_source_item_key first_missing_key first_unfetched_key
  | Partial_fetch { first_unfetched_key; last_source_item_key = _ } ->
      Format.sprintf "Partial: last: %i unfetched: %i" first_unfetched_key
        first_unfetched_key

let pp_progress fmt { total; remaining } =
  Format.fprintf fmt "(%i/%i)" remaining total

let pp_report fmt { status; sync_progress } =
  let status = status_to_string status in
  Format.fprintf fmt "%s%a" status
    (Format.pp_print_option pp_progress)
    sync_progress

let log_status = function
  | Unknown -> Console.info [ "Database status is unknown" ]
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

let check_status ~source idb =
  let open Fut.Syntax in
  let* db_infos = get_db_infos idb in
  let open Fut.Result_syntax in
  let+ total_item_count = fetch_total_item_count source in
  (* Keys start at 0, it's natural to count item by starting with 0 *)
  let last_source_item_key = total_item_count - 1 in
  if not (is_db_consistent ~source ~last_source_item_key db_infos) then
    Inconsistent
  else
    match db_infos with
    | { last_key = None; last_value = None } ->
        (* The db has not yet been populated with placeholders *)
        New_items
          {
            first_missing_key = 0;
            first_unfetched_key = 0;
            last_source_item_key;
          }
    | { last_key = Some key; last_value = item } when key < last_source_item_key
      ->
        (* New items were added to the source since the last sync *)
        let first_unfetched_key =
          match item with None -> 0 | Some { id; _ } -> id + 1
        in
        New_items
          {
            first_missing_key = key + 1;
            first_unfetched_key;
            last_source_item_key;
          }
    | { last_key = Some key; last_value = item } when key = last_source_item_key
      -> (
        match item with
        | Some { id; _ } when id = key -> In_sync
        | Some { id; _ } ->
            Partial_fetch { first_unfetched_key = id + 1; last_source_item_key }
        | None ->
            Partial_fetch { first_unfetched_key = 0; last_source_item_key })
    | _ -> Inconsistent

let update_views source idb =
  let open Fut.Result_syntax in
  let+ views =
    Source.query source
      (module Source.Api.Views)
      { include_external_content = false }
      { user_id = source.auth_response.user.id }
  in
  List.iter views.items ~f:(fun (item : Item.t) ->
      let open Brr_io.Indexed_db in
      let transaction =
        Database.transaction
          [ (module Stores.Collections_store) ]
          ~mode:Readwrite idb
      in
      let s_collections =
        Transaction.object_store (module Stores.Collections_store) transaction
      in
      let _sort_name = Option.value item.sort_name ~default:item.name in
      let _ =
        if
          String.equal "music" @@ Option.get_or ~default:"" item.collection_type
        then
          Stores.Collections_store.put
            { id = Jellyfin item.id; name = item.name }
            s_collections
          |> ignore
      in
      ());
  views

type folder_path = { folder_id : string; path : string }
type view_paths = { view_id : string; paths : folder_path list }

let deduce_virtual_folders_from_views source (views : Views.response) =
  let open Fut.Result_syntax in
  (* Immediate children of a music view are musicartists. But their parent field
     does not contain the view's id the a virtual folder's id. We gather these
     virtual folder thar are required to deduce the view from an item. *)
  let parent_ids_of_view_children { Item.id; _ } =
    let+ res =
      Source.query source
        (module Source.Api.Items)
        Source.Api.Items.
          {
            ids = [];
            parent_id = Some id;
            user_id = source.auth_response.user.id;
            fields = [ ParentId ];
            include_item_types = [];
            start_index = None;
            limit = None;
            sort_order = None;
            sort_by = [];
            recursive = false;
            enable_user_data = false;
            enable_images = false;
          }
        ()
    in
    List.fold_left ~init:String.Set.empty res.items
      ~f:(fun set { Item.parent_id; _ } ->
        match parent_id with
        | None | Some None -> set
        | Some (Some parent_id) -> String.Set.add parent_id set)
  in
  let paths_of_parents parents =
    let+ res =
      Source.query source
        (module Source.Api.Items)
        Source.Api.Items.
          {
            ids = String.Set.to_list parents;
            parent_id = None;
            user_id = source.auth_response.user.id;
            fields = [ Path ];
            include_item_types = [];
            start_index = None;
            limit = None;
            sort_order = None;
            sort_by = [];
            recursive = false;
            enable_user_data = false;
            enable_images = false;
          }
        ()
    in
    List.filter_map res.items ~f:(fun { Item.id; path; _ } ->
        Option.map (fun path -> { folder_id = id; path }) path)
  in
  let open Fut.Syntax in
  let+ result =
    List.map views.items ~f:(fun ({ Item.id; _ } as view) ->
        let open Fut.Result_syntax in
        let* parents = parent_ids_of_view_children view in
        let+ paths = paths_of_parents parents in
        { view_id = id; paths })
    |> Fut.of_list
  in
  Result.flatten_l result

let views_of_path (vfolders : view_paths list) path =
  (* We look at the prefix of a path to determine which virtual_folder (and thus
     view) it's a part of. *)
  List.filter_map vfolders ~f:(fun { view_id; paths } ->
      if
        List.exists paths ~f:(fun { folder_id = _; path = pre } ->
            String.prefix ~pre path)
      then Some view_id
      else None)

let sync ?(report = fun _ -> ()) ~(source : Source.connexion) idb =
  let open Fut.Result_syntax in
  let make_placeholders first last =
    (* todo: error handling *)
    let transaction =
      Database.transaction [ (module OI) ] ~mode:Readwrite idb
    in
    let store = Transaction.object_store (module OI) transaction in
    for i = first to last do
      ignore @@ OI.put { id = i; item = None } store
    done
  in
  let fetch_missing_items first last =
    let open Source in
    let* views = update_views source idb in
    let* vfolders = deduce_virtual_folders_from_views source views in
    let () = Console.info [ "Fetching items"; first; "to"; last; ":" ] in
    let fetch_queue = Queue.create () in
    let total = last - first + 1 in
    let rec enqueue ~start_index todo =
      if todo > 0 then (
        let limit = min todo chunk_size in
        let req =
          Api.Items.
            {
              (* todo make sort explicit (by date added date) *)
              ids = [];
              parent_id = None;
              user_id = source.auth_response.user.id;
              fields = [ ParentId; Path; Genres ];
              include_item_types;
              start_index = Some start_index;
              limit = Some limit;
              sort_order = Some Ascending;
              sort_by = [ DateCreated ];
              recursive = true;
              enable_user_data = false;
              enable_images = true;
            }
        in
        Queue.add req fetch_queue;
        enqueue ~start_index:(start_index + limit) (todo - limit))
    in
    enqueue ~start_index:first total;
    let total_queries = Queue.length fetch_queue in
    let rec run_queue ?(threads = 2) q =
      assert (threads > 0);
      let rec take_n acc n =
        if n = 0 then List.rev acc
        else
          match Queue.take_opt q with
          | None -> List.rev acc
          | Some elt -> take_n (elt :: acc) (n - 1)
      in
      (* TODO requests are slow: the server takes 3.5 seconds when asking for
         two times 2000 records and handling the result is slow (2~3 seconds).
         We could send the next request while handling the previous results.
         This should speed up the process a lot. (Also there might be
         opportunities to speed up result handling.) *)
      let f req =
        let+ { Api.Items.start_index; items; _ } =
          query source (module Api.Items) req ()
        in
        let () =
          report
          @@ Some
               { total = total_queries; remaining = Queue.length fetch_queue }
        in
        let idb_put ~start_index items =
          let open Brr_io.Indexed_db in
          List.iteri items
            ~f:(fun
                index
                ({ Api.Item.id; path; type_; name; album_id; server_id; _ } as
                 item)
              ->
              let transaction =
                Database.transaction
                  [
                    (module OI);
                    (module I);
                    (module Stores.Collections_store);
                    (module Stores.Genres_store);
                    (module Stores.Albums_store);
                    (module Stores.Tracks_store);
                  ]
                  ~mode:Readwrite idb
              in
              let s_list = Transaction.object_store (module OI) transaction in
              let s_items = Transaction.object_store (module I) transaction in
              let s_collections =
                Transaction.object_store
                  (module Stores.Collections_store)
                  transaction
              in
              let i_collections =
                Stores.(
                  Collections_store.index
                    (module Collections_by_id)
                    ~name:"by-id" s_collections)
              in
              let s_genres =
                Console.log [ "objs" ];
                Transaction.object_store
                  (module Stores.Genres_store)
                  transaction
              in
              let i_genres =
                Stores.Genres_store.index
                  (module Stores.Genres_by_canonical_name)
                  ~name:"genres_by_canon_name" s_genres
              in
              let s_albums =
                Transaction.object_store
                  (module Stores.Albums_store)
                  transaction
              in
              let s_tracks =
                Transaction.object_store
                  (module Stores.Tracks_store)
                  transaction
              in
              let get_or_set_genre name =
                let canon = canonicalize_string name in
                Stores.Genres_by_canonical_name.get_key canon i_genres
                |> Request.fut
                |> Fun.flip Fut.bind (function
                     | Ok (Some key) ->
                         Console.log [ "Found genre with key"; key ];
                         Fut.return key
                     | Ok None ->
                         let genre = Generic_schema.{ Genre.name; canon } in
                         Console.log [ "Genre not found, inserting" ];
                         Stores.Genres_store.add genre s_genres
                         |> Request.fut |> Fut.map Result.get_exn
                     | Error e -> raise (Jv.Error e))
              in
              let prepare_genres genre_items =
                List.concat_map genre_items
                  ~f:(fun ({ name; _ } : Api.Item.genre_item) ->
                    String.split_on_char ~by:';' name
                    |> List.concat_map ~f:(String.split_on_char ~by:',')
                    |> List.map ~f:String.trim
                    |> List.map ~f:get_or_set_genre)
                |> Fut.of_list
              in
              let index = start_index + index in
              let path = Option.value ~default:"" path in
              let views = views_of_path vfolders path in
              let sort_name = Option.value item.sort_name ~default:name in
              ignore (OI.put { id = index; item = Some id } s_list);
              match type_ with
              | MusicAlbum ->
                  prepare_genres item.genre_items
                  |> Fut.map (fun genres ->
                         let key = { Generic_schema.Album.Key.name; genres } in
                         let id = Generic_schema.Id.Jellyfin id in
                         Console.log [ "Album"; key; id; sort_name; genres ];
                         Stores.Albums_store.add ~key { id; sort_name; genres }
                           s_albums)
                  |> ignore
              | Audio ->
                  let views =
                    List.map views ~f:(fun view ->
                        Stores.Collections_by_id.get_key (Jellyfin view)
                          i_collections
                        |> Request.fut)
                    |> Fut.of_list
                    |> Fut.map
                         (List.filter_map ~f:(Result.get_or ~default:None))
                  in
                  Fut.pair views @@ prepare_genres item.genre_items
                  |> Fut.map (fun (collections, genres) ->
                         let key =
                           {
                             Generic_schema.Track.Key.name;
                             genres;
                             collections;
                           }
                         in
                         let id = Generic_schema.Id.Jellyfin id in
                         let server_id = Generic_schema.Id.Jellyfin server_id in
                         let album_id =
                           Option.map
                             (fun id -> Generic_schema.Id.Jellyfin id)
                             album_id
                         in
                         Stores.Tracks_store.add ~key
                           { id; album_id; sort_name; genres; server_id }
                           s_tracks)
                  |> ignore
              | _ ->
                  I.put
                    { sorts = { date_added = index; views; sort_name }; item }
                    s_items
                  |> ignore)
        in
        idb_put ~start_index items
      in
      let reqs = take_n [] threads in
      let open Fut.Syntax in
      let* reqs = Fut.of_list (List.map ~f reqs) in
      if List.is_empty reqs then Fut.ok () else run_queue q
    in
    run_queue fetch_queue
  in
  function
  | New_items { first_missing_key; first_unfetched_key; last_source_item_key }
    ->
      make_placeholders first_missing_key last_source_item_key;
      fetch_missing_items first_unfetched_key last_source_item_key
  | Partial_fetch { first_unfetched_key; last_source_item_key } ->
      fetch_missing_items first_unfetched_key last_source_item_key
  | Inconsistent -> Fut.ok ()
  | _ -> Fut.ok ()

let check_and_sync ?report ~source idb =
  let open Fut.Result_syntax in
  let* status = check_status ~source idb in
  let initial = { initial_report with status } in
  let report' =
    Option.map
      (fun report ->
        let () = (* Send a first report *) report initial in
        fun sync_progress -> report { initial with sync_progress })
      report
  in
  let+ () = sync ?report:report' ~source idb status in
  Option.iter
    (fun report -> report { status = In_sync; sync_progress = None })
    report

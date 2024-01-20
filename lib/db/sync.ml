open! Std
open Brrer
open Brr
open Brr_io.Indexed_db
module Source = Data_source.Jellyfin
module OI = Stores.Orderred_items_store
module I = Stores.Items_store
module VF = Stores.Virtual_folder_store

(* Items Hierarchy

   UserRootFolder ("Media Folders" "e9d5075a555c1cbc394eec4cef295274")
                  (Gets the root folder from a user's library. )
   ^ CollectionFolder ("MusicLib" "150848cd4f44b9ae32ec5a7934de39ce")

   AggregateFolder ("root" "f27caa37e5142225cceded48f6553502")
   ^ Folder ("media" "92fde71d0ec577b531e7b3427b223bed")
   ^ Folder+ <- MusicArtist <- MusicAlbum <- Audio

   Jellyfin considers that Folder ("media" "92fde71d0ec577b531e7b3427b223bed") == CollectionFolder ("MusicLib" "150848cd4f44b9ae32ec5a7934de39ce")

   This equality can be retrived by querying the "VirtualFolders" and look at
   the locations' paths.

   For simplicity (?) we consider that an item is part of a view if its path is
   prefixed by one of the view's virtual folder locations.
*)

let chunk_size = 500

let include_item_types =
  [ Source.Api.Item.Folder; AggregateFolder; MusicArtist; MusicAlbum; Audio ]

let fetch_total_item_count source =
  let open Fut.Result_syntax in
  let+ res =
    Source.query source
      (module Source.Api.Items)
      Source.Api.Items.
        {
          (* todo make sort explicit (by date added date)*)
          user_id = source.auth_response.user.id;
          fields = [];
          include_item_types;
          start_index = None;
          limit = 0;
          recursive = true;
        }
      ()
  in
  res.total_record_count

type db_infos = {
  last_key : int option;
  last_value : Stores.Orderred_items.t option;
}

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
  List.iter views.items ~f:(fun item ->
      let open Brr_io.Indexed_db in
      let transaction =
        Database.transaction [ (module OI); (module I) ] ~mode:Readwrite idb
      in
      let s_items = Transaction.object_store (module I) transaction in
      I.put { sorts = { date_added = -1; views = [] }; item } s_items |> ignore)

let update_virtual_folders source idb =
  let open Fut.Result_syntax in
  let+ vfolders =
    Source.query source (module Source.Api.Virtual_folders) () ()
  in
  List.iter vfolders ~f:(fun vf ->
      let open Brr_io.Indexed_db in
      let transaction =
        Database.transaction [ (module VF) ] ~mode:Readwrite idb
      in
      Transaction.object_store (module VF) transaction |> VF.put vf |> ignore);
  vfolders

let views_of_path vfolders path =
  (* We look at the prefix of a path to determine which virtual_folder (and thus
     view) it's a part of. *)
  List.filter_map vfolders
    ~f:(fun { Source.Api.Virtual_folders.item_id; locations; _ } ->
      if List.exists locations ~f:(fun pre -> String.prefix ~pre path) then
        Some item_id
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
    let* () = update_views source idb in
    let* vfolders = update_virtual_folders source idb in
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
              user_id = source.auth_response.user.id;
              fields = [ ParentId; Path ];
              include_item_types;
              start_index = Some start_index;
              limit;
              recursive = true;
            }
        in
        Queue.add req fetch_queue;
        enqueue ~start_index:(start_index + limit) (todo - limit))
    in
    enqueue ~start_index:first total;
    let total_queries = Queue.length fetch_queue in
    let rec run_queue ?(threads = 1) q =
      assert (threads > 0);
      let rec take_n acc n =
        if n = 0 then List.rev acc
        else
          match Queue.take_opt q with
          | None -> List.rev acc
          | Some elt -> take_n (elt :: acc) (n - 1)
      in
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
          let transaction =
            Database.transaction [ (module OI); (module I) ] ~mode:Readwrite idb
          in
          let s_list = Transaction.object_store (module OI) transaction in
          let s_items = Transaction.object_store (module I) transaction in
          List.iteri items ~f:(fun index ({ Api.Item.id; path; _ } as item) ->
              let index = start_index + index in
              let views = views_of_path vfolders path in
              ignore (OI.put { id = index; item = Some id } s_list);
              ignore
                (I.put { sorts = { date_added = index; views }; item } s_items))
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

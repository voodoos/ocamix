open! Std
open Brrer
open Brr
open Brr_io.Indexed_db
module Source = Data_source.Jellyfin
module OI = Stores.Orderred_items_store
module I = Stores.Items_store

let chunk_size = 500

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
          include_item_types = [ Audio ];
          start_index = None;
          limit = 0;
          recursive = true;
        }
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
  | In_sync
  | Inconsistent
  | New_items of {
      first_missing_key : int;
      first_unfetched_key : int;
      last_source_item_key : int;
    }
  | Partial_fetch of { first_unfetched_key : int; last_source_item_key : int }

let log_status = function
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

type progress = { remaining : int }

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
              fields = [];
              include_item_types = [ Audio ];
              start_index = Some start_index;
              limit;
              recursive = true;
            }
        in
        Queue.add req fetch_queue;
        enqueue ~start_index:(start_index + limit) (todo - limit))
    in
    enqueue ~start_index:first total;
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
          query source (module Api.Items) req
        in
        let () = report { remaining = Queue.length fetch_queue } in
        let idb_put ~start_index items =
          let open Brr_io.Indexed_db in
          let transaction =
            Database.transaction [ (module OI); (module I) ] ~mode:Readwrite idb
          in
          let s_list = Transaction.object_store (module OI) transaction in
          let s_items = Transaction.object_store (module I) transaction in
          List.iteri items ~f:(fun index ({ Api.Item.id; _ } as item) ->
              let index = start_index + index in
              ignore (OI.put { id = index; item = Some id } s_list);
              ignore (I.put { sorts = { date_added = index }; item } s_items))
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
  | _ -> Fut.ok ()

let check_and_sync ?report ~source idb =
  let open Fut.Result_syntax in
  let* status = check_status ~source idb in
  log_status status;
  sync ?report ~source idb status

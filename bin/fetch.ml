open Import

type data =
  | Track of
      Db.Generic_schema.Track.Key.t
      * Db.Generic_schema.Track.t
      * Db.Generic_schema.Album.t option
  | Album of int * Db.Generic_schema.Album.t

let tracks' ranged_view i =
  let open View in
  let view = ranged_view.view in
  let indexes =
    Array.map
      ~f:(fun index ->
        let index = index + view.start_offset in
        Order.apply ~size:view.item_count ranged_view.order index)
      i
  in
  Worker_client.(query Get_tracks (view, indexes))

let tracks ranged_view i =
  let data = tracks' ranged_view i in
  Array.mapi i ~f:(fun i _ ->
      let open Fut.Result_syntax in
      let* data = data in
      match data.(i) with
      | (exception _) | None -> Fut.error (`Msg "No result")
      | Some (k, t, a) -> Fut.ok (Track (k, t, a)))

let album store (ranged_view : View.ranged) keys index =
  let module A_store = Db.Stores.Albums_store in
  let view = ranged_view.view in
  let index = index + view.start_offset in
  try
    let key =
      Bigarray.Array1.get keys
      @@ View.Order.apply ~size:view.item_count ranged_view.order index
      |> Int32.to_int (* this is lossy we could use [Jv.of_int32] *)
    in
    let open Fut.Result_syntax in
    let* album =
      A_store.get key store |> IDB.Request.fut
      |> Fut.map (Result.map_err (fun e -> `Jv e))
    in
    match album with
    | None -> Fut.error (`Msg "No result")
    | Some album -> Fut.ok (Album (key, album))
  with Invalid_argument _ -> Fut.error (`Msg "No result")

let albums db (ranged_view : View.ranged) keys indexes =
  let module A_store = Db.Stores.Albums_store in
  let store =
    IDB.Database.transaction [ (module A_store) ] ~mode:Readonly db
    |> IDB.Transaction.object_store (module A_store)
  in
  let f = album store ranged_view keys in
  Array.map indexes ~f

let view_indexes db (ranged_view : View.ranged) ?keys indexes =
  match ranged_view.view.request.kind with
  | Tracks -> tracks ranged_view indexes
  | Albums ->
      let keys = Option.get_exn_or "TODO" keys in
      albums db ranged_view keys indexes

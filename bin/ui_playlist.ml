open Import
open Brrer
open Brrer.Brr
open! Brr_lwd_ui
open! Brr_lwd
module Api = DS.Api

(** Application part *)

(** Columns declaration *)
let columns () =
  Table.Columns.
    [|
      v "Order" "5rem" @@ [ `P (El.txt' "#") ];
      v "Cover" "5rem" @@ [ `P (El.txt' "Cover") ];
      v "Title" "1fr" @@ [ `P (El.txt' "Title") ];
    |]

let make ~reset_playlist ~fetch (view : (Db.View.t, 'a) Fut.result Lwd.t) =
  let img_url server_id item_id =
    let servers =
      (* should this be reactive ? *)
      Lwd.peek Servers.connexions |> Lwd_seq.to_list
    in
    let url =
      try
        let connexion : DS.connexion = List.assq server_id servers in
        Printf.sprintf "%s/Items/%s/Images/Primary?width=50" connexion.base_url
          item_id
      with Not_found -> "server-error.png"
    in
    At.src (Jstr.v url)
  in
  let render view start_index
      {
        Db.Stores.Items.item =
          { Api.Item.id; name; album_id; server_id; image_blur_hashes; _ };
        _;
      } =
    let play_from _ =
      ignore
        (let open Fut.Result_syntax in
         let+ (view : Db.View.t) = view in
         reset_playlist
           { view with start_offset = view.start_offset + start_index })
    in
    let play_on_click = Elwd.handler Ev.click play_from in
    let img_url =
      match (image_blur_hashes, album_id) with
      | { primary = None }, _ | _, None ->
          Lwd.return (At.src (Jstr.v "track.png"))
      | _, Some id -> Lwd.return (img_url server_id id)
    in
    let status =
      Lwd.map (Lwd.get Player.now_playing) ~f:(function
        | Some { item = { id = item_id; _ }; _ } when String.equal item_id id ->
            El.div ~at:[ At.class' (Jstr.v "playing") ] [ El.txt' "|>" ]
        | Some _ | None -> El.div [ El.txt' (string_of_int (start_index + 1)) ])
    in
    [
      `R status;
      `R
        (Elwd.div
           ~ev:[ `P play_on_click ]
           [ `R (Elwd.img ~at:[ `R img_url; `P (At.width 50) ] ()) ]);
      `P (El.div [ El.span [ El.txt' name ] ]);
    ]
  in
  let placeholder _i = [] in
  let ui_table =
    { Table.table = { columns = columns () }; row_height = Em 4. }
  in
  let data_source =
    Lwd.map view ~f:(fun view ->
        let total_items = Fut.map (Result.map Db.View.item_count) view in
        let fetch i =
          let open Fut.Result_syntax in
          let* view = view in
          fetch view i
        in
        let render = render view in
        { Table.Virtual.total_items; fetch; render })
  in
  Table.Virtual.make ~ui_table ~placeholder data_source

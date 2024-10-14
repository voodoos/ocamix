open Import
open Brrer
open Brrer.Brr
open! Brr_lwd_ui
open! Brr_lwd
module Api = DS.Api

(** Application part *)

(** Columns declaration *)
let columns () =
  Lwd.pure
  @@ Lwd_seq.of_array
       Table.Columns.
         [|
           v "Order" "5rem" @@ [ `P (El.txt' "#") ];
           v "Cover" "5rem" @@ [ `P (El.txt' "Cover") ];
           v "Title" "1fr" @@ [ `P (El.txt' "Title") ];
         |]

let make ~reset_playlist ~fetch ?scroll_target (view : Lwd_view.ordered) =
  let ranged =
    Lwd.map2 (Lwd_view.to_view view) view.order ~f:(fun view order ->
        { View.view; first = 0; last = 0; order })
  in
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
  let render (ranged : View.ranged Lwd.t) start_index
      {
        Db.Stores.Items.item =
          { Api.Item.id; name; album_id; server_id; image_blur_hashes; _ };
        _;
      } =
    let play_from (ranged : View.ranged Lwd.t) =
      Lwd.map ranged ~f:(fun ranged _ ->
          ignore
            (reset_playlist
               {
                 ranged with
                 view =
                   {
                     ranged.view with
                     start_offset = ranged.view.start_offset + start_index;
                   };
               }))
    in
    let play_on_click =
      Lwd.map (play_from ranged) ~f:(fun cb -> Elwd.handler Ev.click cb)
    in
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
           ~ev:[ `R play_on_click ]
           [ `R (Elwd.img ~at:[ `R img_url; `P (At.width 50) ] ()) ]);
      `P (El.div [ El.span [ El.txt' name ] ]);
    ]
  in
  let placeholder _i = [] in
  let ui_table =
    { Table.table = { columns = columns () }; row_height = Em 4. }
  in
  let data_source =
    let total_items = Lwd.map2 view.item_count ~f:( - ) view.start_offset in
    let fetch = Lwd.map ranged ~f:(fun ranged i -> fetch ranged i) in
    let render = Lwd.pure (render ranged) in
    { Table.Virtual.total_items; fetch; render }
  in
  Table.Virtual.make ~ui_table ~placeholder ?scroll_target data_source

let make_now_playing ~reset_playlist ~fetch view =
  let scroll_target = Lwd.get Player.playstate.current_index in
  make ~scroll_target ~reset_playlist ~fetch view

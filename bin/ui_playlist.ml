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
           v "Duration" "5rem" @@ [ `P (El.txt' "Duration") ];
         |]

let make ~reset_playlist ~fetch ?(status = []) ?scroll_target
    (view : Lwd_view.ordered) =
  let ranged =
    Lwd.map2 (Lwd_view.to_view view) view.order ~f:(fun view order ->
        { View.view; first = 0; last = 0; order })
  in
  let img_url server_id album_id =
    let servers =
      (* should this be reactive ? *)
      Lwd.peek Servers.connexions |> Lwd_seq.to_list
    in
    let url =
      let connexion : DS.connexion = List.assq server_id servers in
      Player.cover_var ~base_url:connexion.base_url ~size:50 ~album_id
        ~cover_type:Front
    in
    Lwd.map (Lwd.get url) ~f:(fun url -> At.src (Jstr.v url))
  in
  let render (ranged : View.ranged Lwd.t) start_index
      Db.Generic_schema.Track.(
        ( { Key.name; duration; _ },
          { id = Jellyfin id; server_id = Jellyfin server_id; album_id; _ } )) =
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
    let img_url = img_url server_id album_id in
    let status =
      Lwd.map (Lwd.get Player.now_playing) ~f:(function
        | Some { item = _, { id = Jellyfin item_id; _ }; _ }
          when String.equal item_id id ->
            El.div ~at:[ At.class' (Jstr.v "playing") ] [ El.txt' "|>" ]
        | Some _ | None -> El.div [ El.txt' (string_of_int (start_index + 1)) ])
    in
    let duration = Duration.pp_track_duration duration in
    [
      `R status;
      `R
        (Elwd.div
           ~ev:[ `R play_on_click ]
           [ `R (Elwd.img ~at:[ `R img_url; `P (At.width 50) ] ()) ]);
      `P (El.div [ El.span [ El.txt' name ] ]);
      `P (El.div [ El.span [ El.txt' duration ] ]);
    ]
  in
  let placeholder i =
    [
      `P (El.txt' (string_of_int (i + 1)));
      `P (El.nbsp ());
      `P (El.txt' "Loading...");
      `P (El.nbsp ());
    ]
  in
  let layout = { Table.columns = columns (); status; row_height = Em 4. } in
  let data_source =
    let total_items = Lwd.map2 view.item_count ~f:( - ) view.start_offset in
    let fetch = Lwd.map ranged ~f:(fun ranged i -> fetch ranged i) in
    Table.Virtual.Lazy { total_items; fetch }
  in
  let render = Lwd.pure (render ranged) in
  Table.Virtual.make ~layout ~placeholder ?scroll_target render data_source

let make_now_playing ~reset_playlist ~fetch view =
  let scroll_target = Lwd.get Player.playstate.current_index in
  make ~scroll_target ~reset_playlist ~fetch view

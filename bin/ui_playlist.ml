open Import
open Brrer
open Brrer.Brr
open! Brr_lwd_ui
open! Brr_lwd
module Api = DS.Api

(** Application part *)

(** Columns declaration *)
let columns cover_cell_width =
  Lwd.pure
  @@ Lwd_seq.of_array
       Table.Columns.
         [|
           v "Order" (Rem 5.) @@ [ `P (El.txt' "#") ];
           v "Cover" cover_cell_width @@ [ `P (El.txt' "Cover") ];
           v "Title" (Fr 1.) @@ [ `P (El.txt' "Title") ];
           v "Duration" (Rem 5.) @@ [ `P (El.txt' "Duration") ];
         |]

let make ~reset_playlist ~fetch ?(status = []) ?scroll_target
    (view : Lwd_view.ordered) =
  let ranged =
    Lwd.map2 (Lwd_view.to_view view) view.order ~f:(fun view order ->
        { View.view; first = 0; last = 0; order })
  in
  let img_url ?size server_id album =
    let servers =
      (* should this be reactive ? *)
      Lwd.peek Servers.connexions |> Lwd_seq.to_list
    in
    let size =
      Option.map_or ~default:50
        (fun l -> Css_length.to_px l |> Float.to_int)
        size
    in
    let url =
      let connexion : DS.connexion = List.assq server_id servers in
      Player.get_album_cover_link ~base_url:connexion.base_url ~size
        ~cover_type:Front album
    in
    At.src (Jstr.v url)
  in
  let cover_cell_width = Css_length.Em 4. in
  let cover ranged start_index server_id album =
    let play_on_click =
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
      Lwd.map (play_from ranged) ~f:(fun cb -> Elwd.handler Ev.click cb)
    in
    let width = Css_length.to_px cover_cell_width |> Float.to_int in
    let width, height = (width, width) in
    (* TODO should be flexible *)
    let src = img_url ~size:cover_cell_width server_id album in
    let cover = [ `R (Elwd.img ~at:[ `P src; `P (At.width width) ] ()) ] in
    let blur_hash =
      let open Option in
      let* album = album in
      let+ hash = String.Map.find_opt "Primary" album.blur_hashes in
      let open Brr_canvas in
      let el = El.canvas ~at:[ At.width width; At.height height ] [] in
      let canvas = Canvas.of_el el in
      let () =
        let fut =
          Blue_hashes_worker_client.query Render { hash; w = width; h = height }
        in
        Fut.await fut (function
          | Error _ -> ()
          | Ok data ->
              let image_data =
                C2d.Image_data.create ~data ~w:width ~h:height ()
              in
              let context = C2d.get_context canvas in
              C2d.put_image_data context image_data ~x:0 ~y:0)
      in
      el
    in
    let elts =
      match blur_hash with None -> cover | Some el -> `P el :: cover
    in
    Elwd.div
      ~at:[ `P (At.class' (Jstr.v "grid-cover")) ]
      ~ev:[ `R play_on_click ]
      elts
  in
  let render (ranged : View.ranged Lwd.t) start_index
      Db.Generic_schema.Track.(
        ( { Key.name; duration; _ },
          { id = Jellyfin id; server_id = Jellyfin server_id; _ },
          album )) =
    let status =
      Lwd.map (Lwd.get Player.now_playing) ~f:(function
        | Some { item = _, { id = Jellyfin item_id; _ }, _; _ }
          when String.equal item_id id ->
            El.div ~at:[ At.class' (Jstr.v "playing") ] [ El.txt' "▷" ]
        | Some _ | None -> El.div [ El.txt' (string_of_int (start_index + 1)) ])
    in
    let duration = Duration.pp_track_duration duration in
    Lwd.return
      (Lwd_seq.of_list
         [
           status;
           cover ranged start_index server_id album;
           Lwd.return (El.div [ El.span [ El.txt' name ] ]);
           Lwd.return (El.div [ El.span [ El.txt' duration ] ]);
         ])
  in
  let placeholder i =
    Lwd.return
      (Lwd_seq.of_list
         [
           Lwd.return (El.txt' (string_of_int (i + 1)));
           Lwd.return (El.nbsp ());
           Lwd.return (El.txt' "Loading...");
           Lwd.return (El.nbsp ());
         ])
  in
  let placeholder_grid _i = Lwd.return Lwd_seq.empty in
  let data_source =
    let total_items = Lwd.map2 view.item_count ~f:( - ) view.start_offset in
    let fetch = Lwd.map ranged ~f:(fun ranged i -> fetch ranged i) in
    Table.Data_source.Lazy { total_items; fetch }
  in
  (* TODO: not for the playlist... and move the bind deeper *)
  Lwd.bind (Lwd.get Ui_filters.grid_display) ~f:(function
    | Off ->
        let layout =
          Table.make_fixed_row_height (columns cover_cell_width) ~status
            ~row_height:(Css_length.Em 4.) ()
        in
        let render =
          render ranged |> Table.Virtual.with_placeholder_or_error ~placeholder
        in
        Table.Virtual.make ~layout ?scroll_target render data_source
    | On ->
        let size = Css_length.Em 6. in
        let layout_grid =
          Table.make_fixed_grid ~status ~item_width:size ~row_height:size ()
        in
        let render (ranged : View.ranged Lwd.t) start_index
            Db.Generic_schema.Track.(
              _, { server_id = Jellyfin server_id; _ }, album) =
          let cover = cover ranged start_index server_id album in
          Lwd.return (Lwd_seq.of_list [ cover ])
        in
        let render_grid =
          render ranged
          |> Table.Virtual.with_placeholder_or_error
               ~placeholder:placeholder_grid
        in

        Table.Virtual_grid.make ?scroll_target layout_grid render_grid
          data_source)

let make_now_playing ~reset_playlist ~fetch view =
  let scroll_target = Lwd.get Player.playstate.current_index in
  make ~scroll_target ~reset_playlist ~fetch view

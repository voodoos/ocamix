open! Import
open Common
open Brrer
open! Brr
open! Brr_lwd
open Lwd_infix

let logger = Logger.for_section "virtual grid"

let make (layout : _ Layout.fixed_grid) render (data_source : _ Data_source.t) =
  let state = new_state layout in
  let total_items, fetch =
    match data_source with Lazy { total_items; fetch } -> (total_items, fetch)
  in
  let number_of_items_per_row =
    Lwd.map (Lwd.get state.dom.wrapper_width) ~f:(function
      | Some w ->
          (* Invarient: if state.dom.wrapper_width is not null then
           state.dom.content_div have been filled. *)
          let parent = Utils.Forward_ref.get_exn state.dom.content_div in
          let item_width = Css_length.to_px' parent layout#item_width |> ceil in
          w / Int.of_float item_width
      | None -> 1)
  in
  let total_rows = Lwd.map2 total_items ~f:( / ) number_of_items_per_row in
  let fetch =
    Utils.map3 fetch total_items number_of_items_per_row
      ~f:(fun f total n_per_row ->
        fun lines_to_fetch ->
         Array.flat_map lines_to_fetch ~f:(fun line ->
             let first_item = line * n_per_row in
             let last_item = min (total - 1) (first_item + n_per_row - 1) in
             f
             @@ Array.init (last_item - first_item) ~f:(fun i -> first_item + i)))
  in
  let render = ... in
  Virtual_table.make_lazy' state render
    (Lazy { total_items = total_rows; fetch })

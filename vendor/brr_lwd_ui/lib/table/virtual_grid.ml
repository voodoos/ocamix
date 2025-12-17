open! Import
open Common
open Brrer
open! Brr
open! Brr_lwd

let logger = Logger.for_section "virtual grid"

let make (layout : _ Layout.fixed_grid) ?scroll_target render
    (data_source : _ Data_source.t) =
  let layout =
    object
      method row_height = layout#row_height
      method item_width = layout#item_width
      method columns = Lwd.return Lwd_seq.empty
      method status = layout#status
      method sort_state = layout#sort_state
    end
  in
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
         Array.map lines_to_fetch ~f:(fun line ->
             let first_item = line * n_per_row in
             let last_item = min (total - 1) (first_item + n_per_row - 1) in
             let cols =
               f
                 (Array.init (last_item - first_item) ~f:(fun i ->
                      first_item + i))
             in
             Fut.ok cols))
  in
  let render =
   fun l result ->
    let result = Utils.var_of_fut_opt result in
    Lwd.map (Lwd.get result) ~f:(function
      | Some (Ok data) ->
          Array.mapi
            ~f:(fun c result ->
              Elwd.div [ `S (Lwd_seq.lift (render (l + c) result)) ])
            data
          |> Lwd_seq.of_array
      | _ -> Lwd_seq.element (Lwd.return (El.txt' "Loading")))
  in
  Virtual_table.make_lazy' state ?scroll_target render
    (Lazy { total_items = total_rows; fetch })

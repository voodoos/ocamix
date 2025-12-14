open Import
open Brrer
open Brr
open Brr_lwd

module Data_source = struct
  type ('data, 'error) t =
    | Lazy of {
        total_items : int Lwd.t;
        fetch : (int array -> ('data, 'error) Fut.result array) Lwd.t;
            (** Fetched indices are always contiguous. *)
      }
end

module Dom = struct
  type ('data, 'layout) state = {
    layout : 'layout;
    (* The content_div ref should be initialized with the correct element as
             soon as it is created. It is not reactive per se. *)
    content_div : El.t Utils.Forward_ref.t;
    (* The wrapper_div ref should be initialized with the correct element as
           soon as it is created. It is not reactive per se. *)
    wrapper_div : El.t Utils.Forward_ref.t;
    (* The size of the wrapper is a reactive value that might change during
           execution when the browser is resized or other layout changes are made. *)
    wrapper_width : int option Lwd.var;
    wrapper_height : int option Lwd.var;
    mutable last_scroll_y : float;
  }

  let number_of_fitting_rows_in dom_state h_px =
    let row_height_px = Css_length.to_px dom_state.layout#row_height in
    Float.(of_int h_px / row_height_px |> ceil) |> int_of_float

  let resize_observer state =
    (* We observe the size of the table to re-populate if necessary *)
    Resize_observer.create ~callback:(fun entries _ ->
        let entry = List.hd entries in
        let rect = Resize_observer.Entry.content_rect entry in
        let width = Dom_rect_read_only.width rect in
        let height = Dom_rect_read_only.height rect in
        (match Lwd.peek state.wrapper_width with
        | Some w when w <> width -> Lwd.set state.wrapper_width (Some width)
        | None -> Lwd.set state.wrapper_width (Some width)
        | _ -> ());
        match Lwd.peek state.wrapper_height with
        | Some h when h <> height -> Lwd.set state.wrapper_height (Some height)
        | None -> Lwd.set state.wrapper_height (Some height)
        | _ -> ())

  let with_scroll_position ?at dom_state target_position el =
    let scroll_target =
      Lwd.map target_position ~f:(fun i ->
          let row_height =
            let parent = Utils.Forward_ref.get_exn dom_state.content_div in
            Int.of_float (Css_length.to_px' parent dom_state.layout#row_height)
          in
          Some (Controlled_scroll.Pos (i * row_height)))
    in
    Controlled_scroll.make ?at ~scroll_target el

  let height_n_rows dom_state n =
    let row_size = dom_state.layout#row_height |> Css_length.to_string in
    Printf.sprintf "height: calc(%s * %i);" row_size n

  let make_spacer dom_state n =
    let at =
      [
        At.class' (Jstr.v "lwdui-virtual-table-row");
        At.class' (Jstr.v "row_spacer");
      ]
    in
    let height_n = height_n_rows dom_state n in
    let style = At.style (Jstr.v height_n) in
    El.div ~at:(style :: at) [ El.nbsp () ]

  let make_rows dom_state ~row_count spaced_rows =
    let table_body =
      Lwd.map spaced_rows ~f:(fun (n, s, m) ->
          let result =
            if n > 0 then
              let first_spacer = Lwd.pure @@ make_spacer dom_state n in
              Lwd_seq.(concat (element first_spacer) s)
            else s
          in
          if m > 0 then
            let last_spacer = Lwd.pure @@ make_spacer dom_state m in
            Lwd_seq.(concat result (element last_spacer))
          else result)
    in
    let total_height =
      Lwd.map row_count ~f:(fun n ->
          Attrs.O.A (height_n_rows dom_state n |> Jstr.v |> At.style))
    in
    let at =
      Attrs.O.(`R total_height @:: v (`P (C "lwdui-lazy-table-content")))
    in
    let on_create el = Utils.Forward_ref.set_exn dom_state.content_div el in
    Elwd.div ~at ~on_create [ `S (Lwd_seq.lift table_body) ]

  let make_wrapper dom_state ?(on_create = Fun.id) ?scroll_target scroll_handler
      rows =
    let at = Attrs.O.(v (`P (C "lwdui-lazy-table-content-wrapper"))) in

    let ev = [ `R scroll_handler ] in
    let observer = resize_observer dom_state in
    let on_create el =
      Utils.Forward_ref.set_exn dom_state.wrapper_div el;
      Resize_observer.observe observer el;
      on_create ()
    in
    let wrapper = Elwd.div ~at ~ev ~on_create [ `R rows ] in
    match scroll_target with
    | Some scroll_target -> with_scroll_position dom_state scroll_target wrapper
    | None -> wrapper

  let make_table dom_state content =
    let table_header =
      Layout.header dom_state.layout#sort_state dom_state.layout
    in
    let table_status = Layout.status dom_state.layout in
    let at = Attrs.to_at @@ Attrs.classes [ "lwdui-lazy-table" ] in
    let grid_style = Layout.style dom_state.layout in
    let s = Lwd.map grid_style ~f:(fun s -> At.style (Jstr.v s)) in
    let at = `R s :: at in
    Elwd.div ~at [ `R table_header; `R content; `R table_status ]

  let make dom_state ?scroll_target scroll_handler rows =
    let wrapper = make_wrapper dom_state ?scroll_target scroll_handler rows in
    make_table dom_state wrapper
end

type ('data, 'error) row_data = {
  index : int;
  content : ('data, 'error) Fut.result option;
}

type ('layout, 'data, 'error) state = {
  dom : ('data, 'layout) Dom.state;
  (* The cache is some sort of LRU to keep live the content of recently seen
 rows *)
  mutable cache : (int, ('data, 'error) row_data Lwd_table.row) Lru.t;
  table : ('data, 'error) row_data Lwd_table.t;
  (* The [row_index] table is used to provide fast random access to the table's
     rows in the observer's callback *)
  row_index : (int, ('data, 'error) row_data Lwd_table.row) Hashtbl.t;
}

let new_cache () =
  let unload row =
    Lwd_table.get row
    |> Option.iter (fun row_data ->
        Lwd_table.set row { row_data with content = None })
  in
  Lru.create ~on_remove:(fun _i row -> unload row) 150

let new_state layout =
  {
    dom =
      {
        layout;
        content_div = Utils.Forward_ref.make ();
        wrapper_div = Utils.Forward_ref.make ();
        wrapper_width = Lwd.var None;
        wrapper_height = Lwd.var None;
        last_scroll_y = 0.;
      };
    cache = new_cache ();
    table = Lwd_table.make ();
    row_index = Hashtbl.create 2048;
  }

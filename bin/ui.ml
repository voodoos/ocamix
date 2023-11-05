open Std
open Brr_lwd_ui

module Two_state_button = struct
  let base = Attrs.classes [ "button" ]

  let at = function
    | Button.On -> Attrs.classes [ "on" ]
    | Off -> Attrs.classes [ "off" ]

  let make ~on_click =
    let on_click = Button.handler Brr.Ev.click on_click in
    Button.two_state ~base ~at ~ev:[ `P on_click ]
end

open Brrer.Brr
open! Brr_lwd

module Draggable_table = struct
  module type Row = sig
    type t

    val render : t -> El.t list
  end

  (** Columns declaration *)
  module Columns = struct
    type column = { name : string; css_size : string }
    type t = column array

    let column name css_size = { name; css_size }

    let to_style t =
      Array.to_string ~sep:" " (fun { css_size; _ } -> css_size) t

    let to_header t =
      let cells =
        Array.fold_right t ~init:[] ~f:(fun { name; _ } acc ->
            El.div [ El.txt' name ] :: acc)
      in
      El.div cells

    let set elt t =
      let value = Jstr.v @@ to_style t in
      El.set_inline_style El.Style.grid_template_columns value elt
  end

  let grid = Attrs.classes [ "draggable-table" ]

  let render_row (type t) (module Row : Row with type t = t) ~dragged_row ~n
      (t_row : t Lwd_table.row) row =
    let add_hint ~top el =
      let c = if top then "hover-top" else "hover-bottom" in
      El.set_class (Jstr.v c) true el
    in
    let remove_hints el =
      List.iter [ "hover-top"; "hover-bottom" ] ~f:(fun c ->
          El.set_class (Jstr.v c) false el)
    in
    let is_on_top e =
      let mouse_ev = Ev.Drag.as_mouse @@ Ev.as_type e in
      let offset_y = Ev.Mouse.offset_y mouse_ev in
      let target = Ev.current_target e |> Ev.target_to_jv |> El.of_jv in
      let size_h = El.inner_h target in
      offset_y <. size_h /. 2.
    in
    Lwd_seq.element
    @@
    let cells = Row.render row in
    assert (List.length cells = n);
    let on_drag_start =
      Elwd.handler Ev.dragstart (fun e ->
          dragged_row := Some t_row;
          Console.log [ "dragstart"; e ])
    in
    let on_drag_over =
      Elwd.handler Ev.dragover (fun e ->
          let target = Ev.current_target e |> Ev.target_to_jv |> El.of_jv in
          remove_hints target;
          let top = is_on_top e in
          let noop =
            let open Option in
            let+ row = !dragged_row in
            let equal_prev row =
              Option.map_or ~default:false (Equal.physical row)
                (Lwd_table.prev t_row)
            in
            let equal_next row =
              Option.map_or ~default:false (Equal.physical row)
                (Lwd_table.next t_row)
            in
            Equal.physical t_row row
            || (top && equal_prev row)
            || ((not top) && equal_next row)
          in
          if not (Option.value ~default:false noop) then (
            Console.log [ "dragover"; target; noop ];
            add_hint ~top target;
            (* > If the mouse is released over an element that is a valid drop target,
               > that is, one that cancelled the last dragenter or dragover event,
               > then the drop will be successful.
               https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/Drag_operations#specifying_drop_targets *)
            Ev.prevent_default e))
    in
    let on_drag_leave =
      Elwd.handler Ev.dragleave (fun e ->
          let target = Ev.current_target e |> Ev.target_to_jv |> El.of_jv in
          remove_hints target)
    in
    let on_drop =
      Elwd.handler Ev.drop (fun e ->
          let target = Ev.current_target e |> Ev.target_to_jv |> El.of_jv in
          remove_hints target;
          let _ =
            let open Option in
            let* row = !dragged_row in
            let+ set = Lwd_table.get row in
            if is_on_top e then ignore @@ Lwd_table.before ~set t_row
            else ignore @@ Lwd_table.after ~set t_row;
            Lwd_table.remove row
          in
          dragged_row := None;
          Console.log [ "DROP"; target; row ])
    in
    Elwd.div
      ~at:[ `P (At.draggable @@ Jstr.v "true") ]
      ~ev:[ `P on_drag_start; `P on_drag_over; `P on_drag_leave; `P on_drop ]
      (List.map cells ~f:(fun c -> `P c))

  let make (type t) (module Row : Row with type t = t) ~columns
      ?(rows : t list Lwd.t = Lwd.return []) () =
    let open Lwd_infix in
    let at = Attrs.to_at grid in
    let$* rows = rows in
    let t_rows = Lwd_table.make () in
    let () =
      List.iter rows ~f:(fun set -> ignore @@ Lwd_table.append ~set t_rows)
    in
    let table_header = Columns.to_header columns in
    let dragged_row = ref None in
    let table_body =
      Lwd_table.map_reduce
        (render_row (module Row) ~dragged_row ~n:(Array.length columns))
        Lwd_seq.monoid t_rows
    in
    let$ elt = Elwd.div ~at [ `P table_header; `S (Lwd_seq.lift table_body) ] in
    Columns.set elt columns;
    elt
end

let playlist_columns =
  Draggable_table.Columns.
    [| column "FirstCOl" "256px"; column "Second" "256px" |]

module Playlist_row = struct
  type t = string * string

  let render (l, r) = [ El.div [ El.txt' l ]; El.div [ El.txt' r ] ]
end

let draggable_table =
  Draggable_table.make
    (module Playlist_row)
    ~columns:playlist_columns
    ~rows:(Lwd.pure [ ("toto", "r1"); ("tata", "r2"); ("titi", "r3") ])

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
    let table_body =
      Lwd_table.map_reduce
        (fun _t_row row ->
          Lwd_seq.element @@
              let cells = Row.render row in
              assert (List.length cells = Array.length columns);
              let on_drag_start =
                Elwd.handler Ev.dragstart (fun e -> Console.log [ "dragstart"; e ])
              in
              let on_drag_enter =
                Elwd.handler Ev.dragenter (fun e ->
                  let mouse_ev = Ev.Drag.as_mouse @@ Ev.as_type e in
                  let offset_y = Ev.Mouse.offset_y mouse_ev in
                  let size_h = Ev.target e in

                  Console.log [ "dragenter"; e ];
                  Console.log [ "dragenter"; offset_y; size_h])
              in
              Elwd.div
                ~at:[ `P (At.draggable @@ Jstr.v "true") ]
                ~ev:[ `P on_drag_start; `P on_drag_enter ]
                (List.map cells ~f:(fun c -> `P c)))
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
  type t = string

  let render t = [ El.div [ El.txt' t ]; El.div [ El.txt' "right" ] ]
end

let draggable_table =
  Draggable_table.make
    (module Playlist_row)
    ~columns:playlist_columns
    ~rows:(Lwd.pure [ "toto"; "tata " ])

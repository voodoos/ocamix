open Std
open Brrer
open Brr
open Brr_lwd

module Columns = struct
  type column = { name : string; css_size : string; content : Elwd.t Elwd.col }
  type t = column array

  let v name css_size content = { name; css_size; content }

  let to_header t =
    let cells =
      Array.fold_right t ~init:[] ~f:(fun { content; _ } acc ->
          `R (Elwd.div content) :: acc)
    in
    cells

  let style t =
    let template =
      Array.to_string ~sep:" " (fun { css_size; _ } -> css_size) t
    in
    Printf.sprintf "%s: %s;" "grid-template-columns" template
end

type t = { columns : Columns.t }
type fixed_row_height = { table : t; row_height : Utils.Unit.t }

let style t =
  let style = Columns.style t.table.columns in
  (* let auto_row =
       Printf.sprintf "%s: %s;" "grid-auto-rows"
       @@ Utils.Unit.to_string t.row_height
     in *)
  String.concat ~sep:" " [ style ]

let header t =
  let row_height = Utils.Unit.to_string t.row_height in
  let at =
    [ `P (At.style (Jstr.v @@ Printf.sprintf "height: %s" row_height)) ]
  in
  Elwd.div ~at @@ Columns.to_header t.table.columns

let _ = Utils.Unit.to_px (Rem 4.)

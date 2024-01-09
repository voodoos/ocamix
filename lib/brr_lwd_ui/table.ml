open Std
open Brrer
open Brr
open Brr_lwd

module Columns = struct
  type column = { name : string; css_size : string; content : Elwd.t Elwd.col }
  type t = column array

  let v name css_size content = { name; css_size; content }

  let to_header t =
    let style = `P (At.style (Jstr.v @@ Printf.sprintf "grid-row: 1/2")) in
    let cells =
      Array.fold_right t ~init:[] ~f:(fun { content; _ } acc ->
          `R (Elwd.div content) :: acc)
    in
    Elwd.div ~at:[ style ] cells

  let set_style t elt =
    let template =
      Array.to_string ~sep:" " (fun { css_size; _ } -> css_size) t
    in
    let value = Jstr.v template in
    El.set_inline_style El.Style.grid_template_columns value elt;
    elt
end

open Import
open Brr
open Brr_lwd

module Columns = struct
  type column = { name : string; css_size : string; content : Elwd.t Elwd.col }
  type t = column Lwd_seq.t Lwd.t

  let v name css_size content = { name; css_size; content }

  let to_header t =
    Lwd_seq.fold_monoid
      (fun { content; _ } -> Lwd_seq.element (Elwd.div content))
      Lwd_seq.monoid t

  let style t =
    let open Lwd_infix in
    let$ template =
      Lwd_seq.fold_monoid
        (fun { css_size; _ } -> css_size)
        ("", fun s s' -> Printf.sprintf "%s %s" s s')
        t
    in

    Printf.sprintf "%s: %s;" "grid-template-columns" template
end

type t = { columns : Columns.t }
type fixed_row_height = { table : t; row_height : Utils.Unit.t }

let style t = Columns.style t.table.columns

let header t =
  let row_height = Utils.Unit.to_string t.row_height in
  let at =
    [
      `P (At.style (Jstr.v @@ Printf.sprintf "height: %s" row_height));
      `P (At.class' (Jstr.v "lwdui-lazy-table-header"));
      `P (At.class' (Jstr.v "lwdui-virtual-table-row"));
    ]
  in
  Elwd.div ~at [ `S (Columns.to_header t.table.columns |> Lwd_seq.lift) ]

let _ = Utils.Unit.to_px (Rem 4.)

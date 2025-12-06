open Import
open Brr
open Brr_lwd

module Columns = struct
  type column = {
    name : string;
    css_size : Css_lenght.t;
    header : Elwd.t Elwd.col;
  }

  type t = column Lwd_seq.t Lwd.t

  let v name css_size header = { name; css_size; header }

  let to_header t =
    Lwd_seq.fold_monoid
      (fun { header; _ } -> Lwd_seq.element (Elwd.div header))
      Lwd_seq.monoid t

  let style t =
    let open Lwd_infix in
    let$ template =
      Lwd_seq.fold_monoid
        (fun { css_size; _ } -> Css_lenght.to_string css_size)
        ("", fun s s' -> Printf.sprintf "%s %s" s s')
        t
    in

    Printf.sprintf "%s: %s;" "grid-template-columns" template
end

type fixed_row_height = {
  columns : Columns.t;
  status : Elwd.t Elwd.col;
  row_height : Css_lenght.t;
}

let style t = Columns.style t.columns

let header t =
  let row_height = Css_lenght.to_string t.row_height in
  let at =
    [
      `P (At.style (Jstr.v @@ Printf.sprintf "height: %s" row_height));
      `P (At.class' (Jstr.v "lwdui-lazy-table-header"));
      `P (At.class' (Jstr.v "lwdui-virtual-table-row"));
    ]
  in
  Elwd.div ~at [ `S (Columns.to_header t.columns |> Lwd_seq.lift) ]

let status t =
  let at = [ `P (At.class' (Jstr.v "lwdui-virtual-table-status")) ] in
  Elwd.div ~at t.status

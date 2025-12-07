open Import
open Brr
open Brr_lwd
module Sort = Utils.Sort

type 'data on_sort = Do_nothing | Set of 'data Sort.compare

module Sort_button_state = struct
  type t = Asc | Desc | None

  let default = None
  let next = function Asc -> Desc | Desc -> None | None -> Asc
end

type 'data sort_state = { column_id : int; compare : 'data Sort.compare }

module Columns = struct
  let stamp = ref 0

  type 'data column = {
    id : int;
    name : string;
    css_size : Css_lenght.t;
    header : Elwd.t Elwd.col;
    on_sort : 'data on_sort;
  }

  type 'data t = 'data column Lwd_seq.t Lwd.t

  let v name css_size ?(on_sort = Do_nothing) header =
    let id = !stamp in
    incr stamp;
    { id; name; css_size; header; on_sort }

  let sort_button ~ev () =
    Button.with_state
      (module Sort_button_state)
      ~ev
      (function
        | Asc -> [ `P (El.txt' "^") ]
        | Desc -> [ `P (El.txt' "v") ]
        | None -> [ `P (El.txt' "-") ])

  let to_header (type data) (compare_state : data sort_state option Lwd.var)
      (t : data t) =
    Lwd_seq.fold_monoid
      (fun { id; header; on_sort; _ } ->
        let sort_button =
          match on_sort with
          | Do_nothing -> None
          | Set compare ->
              let ev =
                Button.handler Ev.click (fun state _ ->
                    let sort = { column_id = id; compare } in
                    let sort =
                      match Sort_button_state.next state with
                      | Desc ->
                          Some { sort with compare = Sort.reverse compare }
                      | Asc -> Some sort
                      | None -> None
                    in
                    Lwd.set compare_state sort;
                    Next)
              in
              Option.some @@ sort_button ~ev:[ `P ev ] ()
        in
        let header =
          match sort_button with
          | None -> header
          | Some (button, _, _) -> `R button :: header
        in
        Lwd_seq.element (Elwd.div header))
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

type 'data fixed_row_height = {
  columns : 'data Columns.t;
  status : Elwd.t Elwd.col;
  row_height : Css_lenght.t;
  sort_state : 'data sort_state option Lwd.var;
}

let style t = Columns.style t.columns

let header compare_state t =
  let row_height = Css_lenght.to_string t.row_height in
  let at =
    [
      `P (At.style (Jstr.v @@ Printf.sprintf "height: %s" row_height));
      `P (At.class' (Jstr.v "lwdui-lazy-table-header"));
      `P (At.class' (Jstr.v "lwdui-virtual-table-row"));
    ]
  in
  Elwd.div ~at
    [ `S (Columns.to_header compare_state t.columns |> Lwd_seq.lift) ]

let status t =
  let at = [ `P (At.class' (Jstr.v "lwdui-virtual-table-status")) ] in
  Elwd.div ~at t.status

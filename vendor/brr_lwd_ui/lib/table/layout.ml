open Import
open Brrer
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
    css_size : Css_length.t;
    header : Elwd.t Elwd.col;
    on_sort : 'data on_sort;
  }

  type 'data t = 'data column Lwd_seq.t Lwd.t

  let v name css_size ?(on_sort = Do_nothing) header =
    let id = !stamp in
    incr stamp;
    { id; name; css_size; header; on_sort }

  let sort_button ~state ~ev () =
    Button.with_state
      (module Sort_button_state)
      ~state ~ev
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
              let state = Lwd.var Sort_button_state.None in
              let () =
                Utils.tap (Lwd.get compare_state) ~f:(function
                  | Some { column_id; _ } when column_id <> id ->
                      (* If another column is used for sorting, this one is not. *)
                      Lwd.set state None
                  | _ -> ())
              in
              Option.some @@ sort_button ~state ~ev:[ `P ev ] ()
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
        (fun { css_size; _ } -> Css_length.to_string css_size)
        ("", fun s s' -> Printf.sprintf "%s %s" s s')
        t
    in

    Printf.sprintf "%s: %s;" "grid-template-columns" template
end

class type ['data] common = object
  method row_height : Css_length.t
  method status : Elwd.t Elwd.col
  method sort_state : 'data sort_state option Lwd.var
end

type 'data fixed_table = < 'data common ; columns : 'data Columns.t >
type 'data fixed_grid = < 'data common ; item_width : Css_length.t >

let make_fixed_row_height columns ?(status = []) ~row_height
    ?(sort_state = Lwd.var None) () =
  object
    method row_height = row_height
    method columns = columns
    method status = status
    method sort_state = sort_state
  end

let make_fixed_grid ?(status = []) ~item_width ~row_height
    ?(sort_state = Lwd.var None) () =
  object
    method row_height = row_height
    method item_width = item_width
    method status = status
    method sort_state = sort_state
  end

let style t = Columns.style t#columns

let header compare_state t =
  let row_height = Css_length.to_string t#row_height in
  let at =
    [
      `P (At.style (Jstr.v @@ Printf.sprintf "height: %s" row_height));
      `P (At.class' (Jstr.v "lwdui-lazy-table-header"));
      `P (At.class' (Jstr.v "lwdui-virtual-table-row"));
    ]
  in
  Elwd.div ~at
    [ `S (Columns.to_header compare_state t#columns |> Lwd_seq.lift) ]

let status t =
  let at = [ `P (At.class' (Jstr.v "lwdui-virtual-table-status")) ] in
  Elwd.div ~at t#status

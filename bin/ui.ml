open Import
open Brrer
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

  let render_row (type t) (module Row : Row with type t = t) ~shared_drag_data
      ~n (t_row : t Lwd_table.row) row =
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
    let get_ev_target e = Ev.current_target e |> Ev.target_to_jv |> El.of_jv in
    Lwd_seq.element
    @@
    let cells = Row.render row in
    assert (List.length cells = n);
    let on_drag_start =
      Elwd.handler Ev.dragstart (fun _ ->
          Document.body G.document |> El.set_class (Jstr.v "dragging") true;
          Lwd.set shared_drag_data @@ Some t_row)
    in
    let on_drag_over =
      Elwd.handler Ev.dragover (fun e ->
          let target = get_ev_target e in
          remove_hints target;
          let top = is_on_top e in
          let noop =
            let open Option in
            let+ row = Lwd.peek shared_drag_data in
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
            add_hint ~top target;
            (* > If the mouse is released over an element that is a valid drop target,
               > that is, one that cancelled the last dragenter or dragover event,
               > then the drop will be successful.
               https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/Drag_operations#specifying_drop_targets *)
            Ev.prevent_default e))
    in
    let on_drag_leave =
      Elwd.handler Ev.dragleave (fun e -> remove_hints @@ get_ev_target e)
    in
    let on_drag_end =
      Elwd.handler Ev.dragend (fun _ ->
          Document.body G.document |> El.set_class (Jstr.v "dragging") false)
    in
    let on_drop =
      Elwd.handler Ev.drop (fun e ->
          remove_hints @@ get_ev_target e;
          let _ =
            let open Option in
            let* row = Lwd.peek shared_drag_data in
            let+ set = Lwd_table.get row in
            if is_on_top e then ignore @@ Lwd_table.before ~set t_row
            else ignore @@ Lwd_table.after ~set t_row;
            Lwd_table.remove row
          in
          Lwd.set shared_drag_data None)
    in
    Elwd.div
      ~at:[ `P (At.draggable @@ Jstr.v "true") ]
      ~ev:
        [
          `P on_drag_start;
          `P on_drag_over;
          `P on_drag_leave;
          `P on_drag_end;
          `P on_drop;
        ]
      (List.map cells ~f:(fun c -> `P c))

  let make (type t) (module Row : Row with type t = t) ~columns
      ?(table : t Lwd_table.t = Lwd_table.make ())
      ?(shared_drag_data : t Lwd_table.row option Lwd.var = Lwd.var None) () =
    let at = Attrs.to_at grid in
    let table_header = Columns.to_header columns in
    let table_body =
      Lwd_table.map_reduce
        (render_row (module Row) ~shared_drag_data ~n:(Array.length columns))
        Lwd_seq.monoid table
    in
    let elt =
      let open Lwd_infix in
      let$ elt =
        Elwd.div ~at [ `P table_header; `S (Lwd_seq.lift table_body) ]
      in
      let () = Columns.set elt columns in
      elt
    in
    (elt, table)
end

let playlist_columns =
  Draggable_table.Columns.
    [| column "FirstCOl" "256px"; column "Second" "256px" |]

module Playlist_row = struct
  type t = string * string

  let render (l, r) = [ El.div [ El.txt' l ]; El.div [ El.txt' r ] ]
end

let make_table rows =
  let table = Lwd_table.make () in
  List.iter rows ~f:(fun set -> ignore @@ Lwd_table.append ~set table);
  table

let draggable_table ?shared_drag_data () =
  Draggable_table.make
    (module Playlist_row)
    ~columns:playlist_columns ?shared_drag_data
    ~table:
      (make_table
         [
           ("toto", "r1");
           ("tata", "r2");
           ("titi", "r3");
           ("ta", "r4");
           ("tu", "r5");
         ])
    ()

(* TODO, we want uniqueness and bubbling up !*)
module Uniqueue (O : Set.OrderedType) = struct
  module Set = Set.Make (O)

  type nonrec t = { mutable queue : O.t Queue.t; mutable uniq : Set.t }

  let create () =
    let queue = Queue.create () in
    let uniq = Set.empty in
    { queue; uniq }

  let add v t =
    let new_elt = not (Set.mem v t.uniq) in
    let () =
      if new_elt then
        let () = Queue.add v t.queue in
        t.uniq <- Set.add v t.uniq
      else
        (* If the element is already in the queue we "bubble" it up *)
        (* Todo: this is not made in a very efficient way... *)
        let new_queue = Queue.create () in
        Queue.iter
          (fun v' ->
            if not @@ Int.equal (O.compare v v') 0 then Queue.add v' new_queue)
          t.queue;
        Queue.add v new_queue;
        t.queue <- new_queue
    in
    new_elt

  let take t =
    let i = Queue.take t.queue in
    t.uniq <- Set.remove i t.uniq;
    i

  let length t = Queue.length t.queue
end

module Int_uniqueue = Uniqueue (Int)

type row_data = { index : int; content : El.t option; el : El.t }

let lazy_table ?(_batch_size = 10) ~total ~(fetch : int -> (El.t, _) Fut.result)
    (*
      ~(_render : 'a -> El.t) *) () =
  ignore fetch;
  let table : row_data Lwd_table.t = Lwd_table.make () in
  (* The [rows] table is used to relate divs to the table's rows in the
     observer's callback *)
  let row_index = Hashtbl.create 2048 in
  let unload_queue = Int_uniqueue.create () in

  let add ?(max_items = 200) i =
    let unload i =
      let open Option.Infix in
      (let* row = Hashtbl.get row_index i in
       let+ row_data = Lwd_table.get row in
       Lwd_table.set row { row_data with content = None })
      |> ignore
    in
    let load i =
      let open Option.Infix in
      (let* row = Hashtbl.get row_index i in
       let+ row_data = Lwd_table.get row in
       let open Fut.Result_syntax in
       let+ el = fetch row_data.index in
       Lwd_table.set row { row_data with content = Some el })
      |> ignore
    in
    let cleanup () =
      let q_length = Int_uniqueue.length unload_queue in
      if q_length > max_items then
        for _ = max_items to q_length do
          let index = Int_uniqueue.take unload_queue in
          unload index
        done
    in

    if Int_uniqueue.add i unload_queue then (
      load i;
      cleanup ())
  in

  let () =
    for i = 1 to total do
      let uuid = new_uuid_v4 () |> Uuidm.to_string in
      let el =
        El.div
          ~at:[ At.v (Jstr.v "data-index") (Jstr.v uuid) ]
          [ El.txt' (string_of_int i) ]
      in
      let set = { index = i; content = None; el } in
      Hashtbl.add row_index i @@ Lwd_table.append ~set table;
      if i < 50 then add i
    done
  in
  let render _ { content; el; _ } =
    let () =
      match content with
      | Some fetched_el -> El.set_children el [ fetched_el ]
      | None -> El.set_children el [ El.txt' "hidden" ]
    in
    Lwd_seq.element (Lwd.return el)
  in
  let table_body = Lwd_table.map_reduce render Lwd_seq.monoid table in
  let scroll_handler =
    let last_scroll_y = ref 0. in
    fun div ->
      (* todo: debounce *)
      (* todo: adjust bleeding with speed *)
      let height elt =
        let jv = El.to_jv elt in
        Jv.get jv "clientHeight" |> Jv.to_int
      in

      let children = El.children div in
      let scroll_y = El.scroll_y div in
      let direction = if scroll_y >. !last_scroll_y then `Down else `Up in
      let () = last_scroll_y := scroll_y in
      let total_height = height div in
      let num_rows = List.length children in
      let row_height = height @@ List.hd children in
      let number_of_visible_rows = (total_height / row_height) + 1 in
      let bleeding = number_of_visible_rows in
      let first_visible_row =
        int_of_float (scroll_y /. float_of_int row_height) + 1
      in
      let last_visible_row = first_visible_row + number_of_visible_rows in
      let first =
        let bleeding =
          match direction with `Up -> 2 * bleeding | _ -> bleeding / 2
        in
        first_visible_row - bleeding |> max 0
      in
      let last =
        let bleeding =
          match direction with `Down -> 2 * bleeding | _ -> bleeding / 2
        in
        last_visible_row + bleeding |> min num_rows
      in
      for i = first to last do
        (* todo: We do way too much work and rebuild the queue each
           time... it's very ineficient *)
        add ~max_items:(10 * number_of_visible_rows) i
      done
  in
  Elwd.div
    ~ev:
      [
        `P
          (Elwd.handler Ev.scroll (fun ev ->
               let div = Ev.target ev |> Ev.target_to_jv |> El.of_jv in
               scroll_handler div));
      ]
    ~at:
      [
        `P (At.v At.Name.class' (Jstr.v "lazy_table"));
        `P (At.id @@ Jstr.v "lazy_tbl");
      ]
    [ `S (Lwd_seq.lift table_body) ]

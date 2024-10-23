open Brr
open Brr_lwd
open Brr_lwd_ui
open Brr_lwd_ui.Table
open Lwd_infix

let random_state = Random.State.make_self_init ()
let new_uuid_v4 () = Uuidm.v4_gen random_state ()
let yjs_doc = Yjs.Doc.make ()
let awareness = Yjs.Awareness.make yjs_doc
let _ = Quill.register ~path:"modules/cursors" Quill.cursors
let () = Yjs.Awareness.set_user_info awareness ~name:"Narines" ()

let webrtc_provider =
  Yjs.Webrtc_provider.make ~room_name:"testroom5267564" ~awareness
    ~signaling:[ "wss://p2p.u31.fr" ] yjs_doc

let _provider = Yjs.Indexeddb_persistence.make ~doc_name:"zedoc" yjs_doc
let () = Jv.set (Window.to_jv G.window) "yjsdoc" (Jv.repr yjs_doc)
let () = Jv.set (Window.to_jv G.window) "yjsprovider" (Jv.repr webrtc_provider)

type p2p_status = {
  peers : int Lwd.var;
  connected : bool Lwd.var;
  synced : bool Lwd.var;
}
[@@warning "-69"]

let p2p_status =
  let peers = Lwd.var 0 in
  let connected = Lwd.var false in
  let synced = Lwd.var false in
  let _ =
    Yjs.Webrtc_provider.on webrtc_provider Status ~f:(fun { connected = v } ->
        Console.debug [ "Received connected"; Jv.of_bool v ];

        Lwd.set connected v)
  in
  let _ =
    Yjs.Webrtc_provider.on webrtc_provider Synced ~f:(fun { synced = v } ->
        Console.debug [ "Received synced"; Jv.of_bool v ];
        Lwd.set synced v)
  in
  let _ =
    let l = List.length in
    let debounce_adds = ref [] in
    Yjs.Webrtc_provider.on webrtc_provider Peers ~f:(fun peers' ->
        Console.debug [ "Received peers"; peers' ];
        (* For unknown reason, disconnecting peers appear added once more when
           they are webrtc peers. That might be a bug in y-webrtc *)
        let is_webrtc_peer p =
          List.exists (String.equal p) peers'.webrtc_peers
        in
        let remove_from_debounce p =
          let result = ref None in
          debounce_adds :=
            List.filter
              (fun p' ->
                if String.equal p p' then (
                  result := Some p;
                  true)
                else false)
              !debounce_adds;
          !result
        in
        let adds =
          List.fold_left
            (fun acc p ->
              match remove_from_debounce p with
              | None ->
                  if is_webrtc_peer p then debounce_adds := p :: !debounce_adds;
                  acc + 1
              | Some _ -> acc)
            0 peers'.added
        in
        Lwd.peek peers + adds - l peers'.removed |> Lwd.set peers)
  in
  { peers; connected; synced }

let p2p_status_ui =
  let open Attrs.O in
  let color =
    Lwd.map2 (Lwd.get p2p_status.connected) (Lwd.get p2p_status.synced)
      ~f:(fun c s ->
        Attrs.O.C
          (match (c, s) with
          | true, true -> "green"
          | true, false -> "orange"
          | false, _ -> "red"))
  in
  let icon =
    let at = `P (C "p2p_status_icon") @:: v (`R color) in
    Elwd.div ~at [ `P (El.txt' "♼") ]
  in

  let counter =
    Lwd.map (Lwd.get p2p_status.peers) ~f:(fun i -> El.txt' (string_of_int i))
  in
  Elwd.div ~at:(v (`P (C "p2p_status"))) [ `R icon; `R counter ]

module Lwd_map = struct
  type 'a binding = { key : string; value : 'a option Lwd.var }

  type 'a t = {
    table : 'a binding Lwd_table.t;
    map : (string, 'a binding Lwd_table.row) Hashtbl.t;
  }

  let make ?(size = 64) () =
    let table = Lwd_table.make () in
    let map = Hashtbl.create size in
    { table; map }

  let set t key value =
    let row =
      match Hashtbl.find_opt t.map key with
      | Some row_var -> row_var
      | None ->
          let row = Lwd_table.append t.table in
          Hashtbl.replace t.map key row;
          row
    in
    match Lwd_table.get row with
    | Some { key = _; value = var } -> Lwd.set var (Some value)
    | None -> Lwd_table.set row { key; value = Lwd.var (Some value) }

  let get t key =
    let row =
      match Hashtbl.find_opt t.map key with
      | Some row -> row
      | None ->
          (* If there is no such binding we get ready to react to its creation *)
          let row =
            Lwd_table.append ~set:{ key; value = Lwd.var None } t.table
          in
          Hashtbl.replace t.map key row;
          row
    in
    (* Rwos should always be set. An unbound row contains the value None *)
    let { key = _; value } = Lwd_table.get row |> Option.get in
    Lwd.get value

  let get_string t key =
    Lwd.map (get t key) ~f:(function
      | None -> None
      | Some (`Jv jv) -> Some (Jv.to_string jv)
      | Some _ -> assert false)

  let _delete t k =
    match Hashtbl.find_opt t.map k with
    | None -> ()
    | Some row ->
        (* Rows should always be set. An unbound row contains the value None *)
        let { key = _; value } = Lwd_table.get row |> Option.get in
        Lwd.set value None
end

module Indexed_table = struct
  module V = Hector.Poly

  type 'a t = { table : 'a Lwd_table.t; mutable index : 'a Lwd_table.row V.t }

  let make () =
    let table = Lwd_table.make () in
    let index = V.create () in
    { table; index }

  let first t = try Some (V.get t.index 0) with Invalid_argument _ -> None

  let append ?set t =
    let row = Lwd_table.append ?set t.table in
    V.add_last t.index row

  (** [blit_append v i n v'] copies data from the vector segment determined by
      vector [v], index [i], and length [n] at the end of the vector [v']. [v]
      must not be empty. *)
  let blit_append v i n v' =
    Console.debug [ "[blit v(%i) %i %i v'(%i)]"; V.length v; i; n; V.length v' ];
    let v'_size = V.length v' in
    let filler = V.get v 0 in
    V.append_array v' (Array.init n (fun _ -> filler));
    V.blit v i v' v'_size n

  let apply_delta (type value) (t : value t) ~(map : Yjs.Array.value -> value)
      delta =
    let cursor = ref 0 in
    let old_index = t.index in
    let new_index = V.create () in
    let apply_one = function
      | Yjs.Array.Retain i ->
          blit_append old_index !cursor i new_index;
          cursor := !cursor + i
      | Yjs.Array.Delete i ->
          let next =
            ref
            @@
            if !cursor = 0 then Lwd_table.first t.table
            else Lwd_table.next (V.get_last new_index)
          in

          for _i = 1 to i do
            (* We remove the [i] next rows *)
            match !next with
            | None -> assert false
            | Some row ->
                next := Lwd_table.next row;
                Lwd_table.remove row
          done;
          cursor := !cursor + i
      | Yjs.Array.Insert a ->
          (* Three cases:
              1. Insertion at the beginning of an empty table
              2. Insertion at the beginning, before the first row
              3. Insertion after another row *)
          if V.length new_index = 0 then
            match first t with
            | None ->
                (* Case 1: the table is empty *)
                Array.iter
                  (fun value ->
                    let set = map value in
                    append ~set { t with index = new_index })
                  a
            | Some first_row ->
                (* Case 2 *)
                let rows =
                  Array.map
                    (fun value ->
                      let set = map value in
                      Lwd_table.before first_row ~set)
                    a
                in
                V.append_array new_index rows
          else
            (* Case 3 *)
            let last = ref (V.get_last new_index) in
            let rows =
              Array.map
                (fun value ->
                  let set = map value in
                  let row = Lwd_table.after !last ~set in
                  last := row;
                  row)
                a
            in
            V.append_array new_index rows
    in
    Array.iter apply_one delta;
    let remaining = V.length old_index - !cursor in
    if remaining > 0 then blit_append old_index !cursor remaining new_index;
    t.index <- new_index
end

let lwd_of_yjs_map (type value) ~(f : key:string -> Yjs.Map.value -> value) map
    =
  let open Yjs in
  let lwd_map : value Lwd_map.t = Lwd_map.make () in
  ignore
    (Map.fold_entries map
       ~f:(fun key v () -> Lwd_map.set lwd_map key (f ~key v))
       ~init:());

  (* Observe changes *)
  let on_event (e : Map.Event.t) =
    Console.debug [ "On_change"; e ];
    let changes = Map.Event.keys_changes e in
    StringMap.iter
      (fun key -> function
        | {
            Map.Event.action = Add | Update;
            new_value = Some new_value;
            old_value = _;
          } ->
            let new_value = f ~key new_value in
            Lwd_map.set lwd_map key new_value
        | { Map.Event.action = Delete; old_value; _ } ->
            Console.debug
              [ "Key:"; key; "Action: delete"; "Old value:"; old_value ]
        | _ -> assert false)
      changes
  in
  ignore @@ Map.observe map on_event;
  lwd_map

let lwd_of_yjs_array ~f arr =
  let lwd_table = Indexed_table.make () in
  (* Load initial value (maybe we could also rely on the delta here) *)
  Yjs.Array.iter
    ~f:(fun ~index:_ value _array ->
      let set = f value in
      Indexed_table.append ~set lwd_table |> ignore)
    arr;
  (* Observe changes *)
  let on_event (e : Yjs.Array.change Yjs.Event.t) =
    let delta = (Yjs.Event.changes e).delta in
    Indexed_table.apply_delta ~map:f lwd_table delta
  in
  ignore @@ Yjs.Array.observe arr on_event;
  lwd_table

(**

  cell ::= data option

  row ::= ("id" : string) -> cell

  column-info ::=
    {
      "id" : string
      "kind" : "string" | ...
      "name" : string }

  table ::=
    {
      "columns" : column-info[]
      "rows" : row[]
    }

  data ::=
    {
      "kind" : "table" | "text"
      "content" : table | string
    }

  section ::=
    {
      "name" : string
      "data" : data
    }

  page ::= section[]
*)

module S (*Schema*) = struct
  open Yjs

  module Data = struct
    let kind = "kind"
    let content = "content"

    type kind = [ `String | `Bool | `Richtext | `Table ]

    let kind_to_string = function
      | `String -> "string"
      | `Bool -> "bool"
      | `Richtext -> "text"
      | `Table -> "table"

    let kind_of_string = function
      | "string" -> `String
      | "bool" -> `Bool
      | "text" -> `Richtext
      | "table" -> `Table
      | _ -> assert false

    let kind_to_name = function
      | `String -> "String"
      | `Bool -> "Checkbox"
      | `Richtext -> "Richtext"
      | `Table -> "Table"

    let make kind_ content_ =
      let kind_ = kind_to_string kind_ in
      let v = Map.make () in
      Yjs.Doc.transact yjs_doc (fun () ->
          Map.set v ~key:kind (`Jv (Jv.of_string kind_));
          Map.set v ~key:content content_);
      v

    module String = struct
      let make value = make `String (`Jv (Jv.of_string value))
    end

    module Bool = struct
      let make value = make `Bool (`Jv (Jv.of_bool value))
    end

    module Richtext = struct
      let make initial_content =
        let text = Yjs.Text.make ?initial_content () in
        make `Richtext (`Text text)
    end

    module Table = struct
      let kind_v = "table"
      let columns = "columns"
      let rows = "rows"

      module Column_info = struct
        let id = "id"
        let kind = "kind"
        let name = "name"

        let make ~kind:kind_ name_ =
          let kind_ = kind_to_string kind_ in
          let id_ = new_uuid_v4 () |> Uuidm.to_string in
          (* TODO check uniqueness ? *)
          let v = Map.make () in
          Console.log [ "New column kind:"; kind ];
          Yjs.Doc.transact yjs_doc (fun () ->
              Map.set v ~key:id (`Jv (Jv.of_string id_));
              Map.set v ~key:kind (`Jv (Jv.of_string kind_));
              Map.set v ~key:name (`Jv (Jv.of_string name_)));
          (id_, v)
      end

      module Row = struct
        let make cells =
          let v = Map.make () in
          Yjs.Doc.transact yjs_doc (fun () ->
              List.iter
                (fun (key, cell) -> Yjs.Map.set v ~key (`Map cell))
                cells);
          v
      end

      let empty () =
        let v = Map.make () in
        Yjs.Doc.transact yjs_doc (fun () ->
            let columns_ = Array.make () in
            let rows_ = Array.make () in
            Map.set v ~key:columns (`Array columns_);
            Map.set v ~key:rows (`Array rows_));
        make `Table (`Map v)
    end
  end

  module Section = struct
    let id = "id"
    let name = "name"
    let data = "data"

    let make ~name data =
      let v = Map.make () in
      Yjs.Doc.transact yjs_doc (fun () ->
          Map.set v ~key:"name" (`Jv (Jv.of_string name));
          Map.set v ~key:"data" (`Map data));
      v
  end

  module Page = struct
    let content = "page_content"
  end
end
[@@warning "-32"]

type column_info = {
  id : string;
  kind : S.Data.kind Lwd.t;
  name : string Lwd.t;
}
[@@warning "-69"]

type cell = { src : Yjs.Map.t; data : data }
and row = { map : Yjs.Map.t; cells : cell Lwd_seq.t Lwd.t } [@@warning "-69"]

and data =
  | String of string Lwd.t
  | Bool of bool Lwd.t
  | Richtext of Yjs.Text.t
  | Table of {
      columns_src : Yjs.Array.t;
      rows_src : Yjs.Array.t;
      columns : column_info Indexed_table.t;
      table : row Indexed_table.t;
    }

type page_item = { id : string; name : string option Lwd.t; data : data }
[@@warning "-69"]

let lwd_of_yjs_page =
  let sections = Yjs.Doc.get_array yjs_doc S.Page.content in
  let rec lwd_of_cell src = { src; data = lwd_of_data src }
  and lwd_of_column_info = function
    | `Map column_infos ->
        let id =
          match Yjs.Map.get column_infos ~key:S.Data.Table.Column_info.id with
          | Some (`Jv jv) -> Jv.to_string jv
          | _ -> assert false
        in
        let lwd_map = lwd_of_yjs_map ~f:(fun ~key:_ v -> v) column_infos in
        let kind =
          Lwd_map.get_string lwd_map S.Data.Table.Column_info.kind
          |> Lwd.map ~f:(fun v ->
                 Console.log [ "GET"; id; v ];
                 Option.get v)
          |> Lwd.map ~f:S.Data.kind_of_string
        in
        let name =
          Lwd_map.get_string lwd_map S.Data.Table.Column_info.name
          |> Lwd.map ~f:Option.get
        in
        { id; kind; name }
    | _ -> assert false
  and lwd_of_table table =
    let columns_src =
      match Yjs.Map.get table ~key:S.Data.Table.columns with
      | Some (`Array columns) -> columns
      | _ -> assert false
    in
    let columns = lwd_of_yjs_array columns_src ~f:lwd_of_column_info in
    let f value =
      match value with
      | `Map cell_map ->
          (* each row is a map of cells *)
          let get_cell_by_id key =
            match Yjs.Map.get ~key cell_map with
            | Some (`Map map) -> lwd_of_cell map
            | _ -> assert false
          in
          let cells =
            Lwd_table.map_reduce
              (fun _ ({ id; _ } : column_info) ->
                let cell = get_cell_by_id id in
                Lwd_seq.element cell)
              Lwd_seq.monoid columns.table
          in
          { map = cell_map; cells }
      | _ -> assert false
    in
    let rows = Yjs.Map.get table ~key:S.Data.Table.rows in
    match rows with
    | Some (`Array rows_src) ->
        let table = lwd_of_yjs_array ~f rows_src in
        Table { columns_src; rows_src; columns; table }
    | _ -> assert false
  and lwd_of_data map =
    let item = lwd_of_yjs_map ~f:(fun ~key:_ v -> v) map in
    let kind =
      match Yjs.Map.get ~key:S.Data.kind map with
      | Some (`Jv s) -> Jv.to_string s
      | _ -> assert false
    in
    match kind with
    | "string" ->
        String
          (Lwd.map (Lwd_map.get item S.Data.content) ~f:(function
            | Some (`Jv s) -> Jv.to_string s
            | _ -> assert false))
    | "bool" ->
        Bool
          (Lwd.map (Lwd_map.get item S.Data.content) ~f:(function
            | Some (`Jv s) -> Jv.to_bool s
            | _ -> assert false))
    | "text" -> (
        match Yjs.Map.get ~key:S.Data.content map with
        | Some (`Text t) -> Richtext t
        | _ -> assert false)
    | "table" -> (
        match Yjs.Map.get ~key:S.Data.content map with
        | Some (`Map v) -> lwd_of_table v
        | _ -> assert false)
    | _ -> assert false
  in

  let lwd_of_section value =
    match value with
    | `Map map ->
        let item = lwd_of_yjs_map ~f:(fun ~key:_ v -> v) map in
        let id = "" in
        let name = Lwd_map.get_string item S.Section.name in
        let data =
          match Yjs.Map.get map ~key:S.Section.data with
          | Some (`Map data) -> lwd_of_data data
          | _ -> assert false
        in
        { id; name; data }
    | _ -> assert false
  in
  lwd_of_yjs_array ~f:lwd_of_section sections

let render_string_cell ~src (value : string Lwd.t) =
  let current_value = Lwd.var "" in
  let snapshot_value = Lwd.var "" in
  let value =
    let$ s = value in
    Lwd.set current_value s;
    El.txt' s
  in
  let edit_active = Lwd.var false in
  let edit_btn =
    let at = Attrs.O.(v (`P (C "cell-edit-btn"))) in
    let on_click =
      Elwd.handler Ev.click (fun _ ->
          Lwd.set snapshot_value @@ Lwd.peek current_value;
          Lwd.set edit_active true)
    in
    let ev = [ `P on_click ] in
    Elwd.div ~at ~ev [ `P (El.txt' "✏️") ]
  in
  let edit_overlay =
    let visible =
      Lwd.map (Lwd.get edit_active) ~f:(function
        | true -> Attrs.O.A (At.class' (Jstr.v "visible"))
        | false -> A At.void)
    in
    let make_inline_form initial_value =
      let open Brr_lwd_ui.Forms.Form in
      let module Inline_edit = struct
        type t = { value : string Field.validation }

        let default = { value = Empty }

        let fields =
          Lwd.return
            (Lwd_seq.of_list
               [
                 field
                   (Lwd.map initial_value ~f:(fun initial_value ->
                        Field.text_input ~required:false (Some initial_value)))
                   (fun _t v -> { value = v });
                 field (Lwd.pure @@ Field.submit (`P "Update")) (fun t _v -> t);
               ])
      end in
      create
        (module Inline_edit)
        (fun t ->
          Console.log [ "Form submitted:"; t ];
          match t with
          (* FIXME: validation already happened, it's redundant to have to match *)
          | { value = Ok value } ->
              let current_value = Lwd.peek current_value in
              if current_value <> value then (
                Console.log [ current_value; " -> "; value ];
                Yjs.Map.set src ~key:S.Data.content (`Jv (Jv.of_string value)));
              Lwd.set edit_active false
          | _ -> ())
    in
    let form = make_inline_form (Lwd.get snapshot_value) in
    let at = Attrs.O.(`R visible @:: v (`P (C "cell-edit-overlay"))) in
    Elwd.div ~at [ `R form ]
  in
  let at = Attrs.O.(v (`P (C "cell"))) in
  Elwd.div ~at [ `R value; `R edit_btn; `R edit_overlay ]

let render_bool_cell ~src (value : bool Lwd.t) =
  let input_el = Utils.Forward_ref.make () in
  let on_change ev =
    let t = Ev.target ev |> Ev.target_to_jv in
    let checked = Jv.get t "checked" in
    Yjs.Map.set src ~key:S.Data.content (`Jv checked)
  in
  let at =
    let type' = At.type' @@ Jstr.v "checkbox" in
    let checked =
      Lwd.map value ~f:(fun v ->
          let () =
            let el = Utils.Forward_ref.get_exn input_el |> El.to_jv in
            Jv.set el "checked" @@ Jv.of_bool v
          in
          match v with true -> At.checked | false -> At.void)
    in
    [ `P type'; `R checked ]
  in
  let ev = [ `P (Elwd.handler Ev.click on_change) ] in
  let field =
    Elwd.(
      div
        [
          `R (input ~at ~ev ~on_create:(Utils.Forward_ref.set_exn input_el) ());
        ])
  in
  let at = Attrs.O.(v (`P (C "cell"))) in
  Elwd.div ~at [ `R field ]

let render_richtext_cell ~src:_ (value : Yjs.Text.t) =
  let container = El.div [] in
  let at = Attrs.O.(v (`P (C "cell"))) in
  (* For Quill's toolbar to show we need to wrap the element in a container
     before instantiating the editor. *)
  let elt = Elwd.div ~at [ `P container ] in
  let _editor =
    let open Quill in
    let toolbar = Array [ Bold; Italic; Underline ] in
    make ~container @@ config ~theme:Snow ~cursors:true ~toolbar ()
    |> (* TODO is there some cleanup to do ? *) Y_quill.make ~awareness value
  in
  elt

let table_data_source source_rows (content : row Indexed_table.t) =
  let reduce_row (row : row) =
    Lwd_seq.fold_monoid
      (fun { src; data } ->
        let elt =
          match data with
          | String value -> render_string_cell ~src value
          | Bool value -> render_bool_cell ~src value
          | Richtext text -> render_richtext_cell ~src text
          | _ -> assert false
        in
        Lwd_seq.element elt)
      Lwd_seq.monoid row.cells
  in
  let render lwd_table_row row =
    let$ cells = reduce_row row in
    let actions =
      let on_delete_row _ =
        let i =
          (* TODO Virtual_table_bis should use an indexed table and take care of this *)
          Hector.Poly.find (( == ) lwd_table_row) content.index
        in
        Console.log [ "DELETE ROW"; i ];
        Yjs.Array.delete source_rows i 1
      in
      Elwd.button
        ~ev:[ `P (Elwd.handler Ev.click on_delete_row) ]
        ~at:[ `P (At.class' (Jstr.of_string "row-actions")) ]
        [ `P (El.txt' "❌") ]
    in
    Lwd_seq.concat cells @@ Lwd_seq.element actions
  in
  Virtual_bis.{ total_items = Lwd.pure 0; source_rows = content.table; render }

let new_table_column_form columns rows =
  let open Brr_lwd_ui.Forms.Form in
  let module Connect_form = struct
    open Brr_lwd_ui.Forms.Form

    type t = {
      name : string Field.validation;
      kind : S.Data.kind Field.validation;
    }

    let default = { name = Empty; kind = Empty }

    let fields =
      Lwd.return
        (Lwd_seq.of_list
           [
             field
               (Lwd.pure @@ Field.text_input ~required:true (Some "demo"))
               (fun t v -> { t with name = v });
             field
               (let { Forms.Field_select.field = elt; label = _; value } =
                  Forms.Field_select.make ~persist:false
                    { name = "kind"; default = "string"; label = [] }
                    (Lwd.pure
                    @@ Lwd_seq.of_list
                         [
                           ("string", "String");
                           ("bool", "Checkbox");
                           ("text", "Richtext");
                         ])
                in
                Lwd.pure { Field.elt; value; validate = (fun v -> Ok v) })
               (fun t kind ->
                 let kind =
                   Forms.Field.map_validation ~f:S.Data.kind_of_string kind
                 in
                 { t with kind });
             field (Lwd.pure @@ Field.submit (`P "Add column")) (fun t _v -> t);
           ])
  end in
  create
    (module Connect_form)
    (fun t ->
      Console.log [ "Form submitted:"; t ];
      match t with
      (* FIXME: validation already happened, it's redundant to have to match *)
      | { name = Ok name; kind = Ok kind } ->
          Console.log [ "Form valid:"; name ];
          Yjs.Doc.transact yjs_doc (fun () ->
              let key, column_info = S.Data.Table.Column_info.make ~kind name in
              let new_cell =
                match kind with
                | `String -> fun () -> S.Data.String.make ""
                | `Bool -> fun () -> S.Data.Bool.make false
                | `Richtext -> fun () -> S.Data.Richtext.make None
                | _ -> assert false
              in
              Yjs.Array.iter
                ~f:(fun ~index:_ row _ ->
                  match row with
                  | `Map row ->
                      let cell = new_cell () in
                      Yjs.Map.set row ~key (`Map cell)
                  | _ -> assert false)
                rows;
              Yjs.Array.push columns [| `Map column_info |])
      | _ -> ())

let new_table_row_form (columns : column_info Indexed_table.t) rows =
  let module Map = Map.Make (String) in
  let open Brr_lwd_ui.Forms.Form in
  let module Row_form = struct
    open Brr_lwd_ui.Forms.Form

    type t =
      [ `String of string | `Bool of bool | `Richtext of string ]
      Field.validation
      Map.t

    let default = Map.empty

    let fields =
      Lwd_table.map_reduce
        (fun _ { id; kind; name } ->
          (let$* kind = kind in
           match kind with
           | `String ->
               field
                 (Lwd.map name ~f:(fun name ->
                      Field.text_input ~placeholder:name ~required:true None))
                 (fun t v ->
                   Console.log [ "Add map with id "; id ];
                   let v = Field.map_validation ~f:(fun v -> `String v) v in
                   Map.add id v t)
           | `Bool ->
               field
                 (Lwd.map name ~f:(fun _name ->
                      let elt, value =
                        Forms.Field_checkboxes.make_single "" "" [] false
                      in
                      { Field.elt; value; validate = (fun v -> Ok v) }))
                 (fun t v ->
                   let v =
                     Field.map_validation
                       ~f:(function Some _ -> `Bool true | _ -> `Bool false)
                       v
                   in
                   Map.add id v t)
           | `Richtext ->
               field
                 (Lwd.map name ~f:(fun name ->
                      Field.text_input ~placeholder:name ~required:true None))
                 (fun t v ->
                   let v = Field.map_validation ~f:(fun v -> `Richtext v) v in
                   Map.add id v t)
           | _ -> assert false)
          |> Lwd_seq.element)
        Lwd_seq.monoid columns.table
      |> Lwd.map ~f:(fun seq ->
             Lwd_seq.concat seq @@ Lwd_seq.element
             @@ field (Lwd.pure @@ Field.submit (`P "Add row")) (fun t _v -> t))
  end in
  create
    ~at:[ `P (At.class' (Jstr.v "inline-row-form")) ]
    (module Row_form)
    (fun t ->
      let cells =
        Map.fold
          (fun key v acc ->
            match v with
            | Field.Ok (`String v) -> (key, S.Data.String.make v) :: acc
            | Field.Ok (`Bool v) -> (key, S.Data.Bool.make v) :: acc
            | Field.Ok (`Richtext v) ->
                (key, S.Data.Richtext.make (Some v)) :: acc
            | _ -> assert false)
          t []
        |> S.Data.Table.Row.make
      in
      Yjs.Array.push rows [| `Map cells |])

let ui_table ~columns_src ~rows_src names =
  {
    table =
      {
        columns =
          Lwd_seq.map
            (fun ({ name; id; _ } : column_info) ->
              let label = Lwd.map name ~f:El.txt' in
              let delete =
                let on_click _ =
                  let findi id =
                    let exception Found of int in
                    try
                      (* TODO this should be done elsewhere (in Schema ?) *)
                      Yjs.Array.iter columns_src ~f:(fun ~index v _ ->
                          match v with
                          | `Map m -> (
                              match
                                Yjs.Map.get m ~key:S.Data.Table.Column_info.id
                              with
                              | Some (`Jv s)
                                when String.equal id (Jv.to_string s) ->
                                  raise (Found index)
                              | _ -> ())
                          | _ -> ());
                      raise Not_found
                    with Found i -> i
                  in
                  Yjs.Doc.transact yjs_doc (fun () ->
                      Yjs.Array.delete columns_src (findi id) 1;
                      Yjs.Array.iter rows_src ~f:(fun ~index:_ v _ ->
                          match v with `Map m -> Yjs.Map.delete m id | _ -> ()))
                in
                Elwd.button
                  ~ev:[ `P (Elwd.handler Ev.click on_click) ]
                  [ `P (El.txt' "❌") ]
              in
              Columns.v "a" "1fr" [ `R label; `R delete ])
            names;
      };
    row_height = Em 5.;
  }

let render_page_item ({ data; _ } : page_item) =
  match data with
  | Table { columns_src; rows_src; columns; table } ->
      let data_source = table_data_source rows_src table in
      let form = new_table_row_form columns rows_src in
      let column_form = new_table_column_form columns_src rows_src in
      let table =
        let columns =
          Lwd_table.map_reduce
            (fun _ column_info -> Lwd_seq.element column_info)
            Lwd_seq.monoid columns.table
        in
        let ui_table = ui_table ~columns_src ~rows_src columns in
        Virtual_bis.make ~ui_table data_source
      in
      Elwd.div
        ~at:Attrs.O.(v (`P (C "flex")))
        [
          `R column_form;
          `R form;
          `R (Elwd.div ~at:Attrs.O.(v (`P (C "table"))) [ `R table ]);
        ]
  | _ -> failwith "not implemented"

let render_page =
  Lwd_table.map_reduce
    (fun _row data ->
      let div = render_page_item data in
      Lwd_seq.element div)
    Lwd_seq.monoid lwd_of_yjs_page.table

let new_table_form =
  let open Brr_lwd_ui.Forms.Form in
  create
    (module struct
      open Brr_lwd_ui.Forms.Form

      type t = unit

      let default = ()

      let fields =
        Lwd.return
          (Lwd_seq.of_list
             [
               field
                 (Lwd.pure @@ Field.submit (`P "Add table"))
                 (fun () _v -> ());
             ])
    end)
    (fun () ->
      let section = S.Data.Table.empty () |> S.Section.make ~name:"tbl" in
      let page_array = Yjs.Doc.get_array yjs_doc S.Page.content in
      Yjs.Array.push page_array [| `Map section |];
      Console.log [ "Create new table:" ])

let app =
  Elwd.div
    ~at:Attrs.O.(v (`P (C "flex")))
    [ `R p2p_status_ui; `R new_table_form; `S (Lwd_seq.lift render_page) ]

let _ =
  let on_load _ =
    let app = Lwd.observe @@ app in
    let on_invalidate _ =
      ignore @@ G.request_animation_frame
      @@ fun _ -> ignore @@ Lwd.quick_sample app
    in
    El.append_children (Document.body G.document) [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

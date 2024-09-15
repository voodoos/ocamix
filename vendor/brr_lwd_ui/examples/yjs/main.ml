open Brr
open Brr_lwd
open Brr_lwd_ui
open Brr_lwd_ui.Table
open Lwd_infix

let yjs_doc = Yjs.Doc.make ()

let provider =
  Yjs.Webrtc_provider.make ~room_name:"testroom5267564"
    ~signaling:[ "wss://p2p.u31.fr" ] yjs_doc

let _provider = Yjs.Indexeddb_persistence.make ~doc_name:"zedoc" yjs_doc
let () = Jv.set (Window.to_jv G.window) "yjsdoc" (Jv.repr yjs_doc)
let () = Jv.set (Window.to_jv G.window) "yjsprovider" (Jv.repr provider)

let _ =
  Jv.call (Jv.repr provider) "on"
    [|
      Jv.of_string "synced";
      Jv.callback ~arity:1 (fun _ -> Console.log [ "SYNCED!" ]);
    |]

let _ =
  Jv.call (Jv.repr provider) "on"
    [|
      Jv.of_string "status";
      Jv.callback ~arity:1 (fun e -> Console.log [ "STATUS"; e ]);
    |]

let _ =
  Jv.call (Jv.repr provider) "on"
    [|
      Jv.of_string "peers";
      Jv.callback ~arity:1 (fun e -> Console.log [ "PEERS"; e ]);
    |]

(* A data table is stored using multiple YJS shared types:
    - A Map storing general metadata
    - A Array storing column information: id, name, type, etc
    - An Array storing entries in the table. Each element of the array is a Array  of columns. (todo: the implementation does not respect that schema)
*)
module Data_table = struct
  open Yjs

  let init_content v =
    Console.log [ "NEW ARRAY" ];
    let content = Array.make () in
    Map.set v ~key:"content" (`Array content);
    content

  let make () =
    (* TODO: YJS USE TRANSACTIONS *)
    let v = Map.make () in
    let content = init_content v in
    Map.set v ~key:"kind" (`Jv (Jv.of_string "table"));
    (v, content)
end

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
          let last = V.get_last new_index in
          for _i = 1 to i do
            (* We remove the [i] next rows *)
            match Lwd_table.next last with
            | None -> assert false
            | Some _row -> Lwd_table.remove last
          done;
          cursor := !cursor + i
      | Insert a ->
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

type cell = String of string

type page_item_data =
  | Table of Yjs.Array.t option * cell Indexed_table.t Indexed_table.t

type page_item = { id : string; data : page_item_data } [@@warning "-69"]

let lwd_of_yjs_page =
  let page_content = Yjs.Doc.get_array yjs_doc "page_content" in
  let f value =
    match value with
    | `Map map ->
        let item = lwd_of_yjs_map ~f:(fun ~key:_ v -> v) map in
        let id = "" in
        let$ yjs, data =
          Lwd.bind (Lwd_map.get_string item "kind") ~f:(function
            | Some "table" -> (
                (* let$ columns = Lwd_map.get item "columns" in *)
                let$ content = Lwd_map.get item "content" in
                let f value =
                  match value with
                  | `Array columns ->
                      (* Each row is an array of columns *)
                      lwd_of_yjs_array
                        ~f:(function
                          | `Jv jv ->
                              (* todo there are more to life than strings *)
                              String (Jv.to_string jv)
                          | _ -> assert false)
                        columns
                  | _ -> assert false
                in
                match content with
                | Some (`Array v) -> (Some v, lwd_of_yjs_array ~f v)
                | _ -> (None, Indexed_table.make ()))
            | _ -> assert false)
        in
        { id; data = Table (yjs, data) }
    | _ -> assert false
  in
  lwd_of_yjs_array ~f page_content

let table_data_source (content : cell Indexed_table.t Indexed_table.t) =
  let reduce_row tbl =
    Lwd_table.map_reduce
      (fun _row value ->
        let elt =
          let value =
            match value with
            | String s ->
                Console.log [ ": string"; s ];
                Lwd_seq.element @@ El.txt' s
          in
          Elwd.div [ `S (Lwd.pure value) ]
        in
        Lwd_seq.element elt)
      Lwd_seq.monoid tbl
  in
  Virtual_bis.
    {
      total_items = Lwd.pure 0;
      source_rows = content.table;
      render =
        (fun _ map ->
          Console.log [ "Value: map"; map ];
          reduce_row map.table);
    }

let table () =
  {
    table =
      {
        columns =
          [|
            Columns.v "a" "5em" [ `P (El.txt' "id") ];
            Columns.v "a" "1fr"
              [ `P (El.txt' "some well-though-of column name") ];
          |];
      };
    row_height = Em 5.;
  }

let add_row content i id v =
  let open Yjs in
  let row = Yjs.Array.make () in
  Array.push row [| `Jv (Jv.of_string id) |];
  Array.push row [| `Jv (Jv.of_string v) |];
  Console.debug [ "Inserting row"; v ];
  Yjs.Array.insert content i [| `Array row |]

let new_table_row_form yjs_array =
  let open Brr_lwd_ui.Forms.Form in
  let module Connect_form = struct
    open Brr_lwd_ui.Forms.Form

    type t = {
      index : int Field.validation;
      id : string Field.validation;
      value : string Field.validation;
    }

    let default = { index = Empty; id = Empty; value = Empty }

    let fields =
      Lwd.return
        (Lwd_seq.of_list
           [
             field
               (Lwd.pure @@ Field.text_input ~required:true (Some "0"))
               (fun t v ->
                 let index =
                   Field.map_validation
                     ~f:(fun v ->
                       int_of_string_opt v |> Option.value ~default:0)
                     v
                 in
                 { t with index });
             field
               (Lwd.pure @@ Field.text_input ~required:true (Some "demo"))
               (fun t v -> { t with id = v });
             field
               (Lwd.pure @@ Field.text_input ~required:false None)
               (fun t v -> { t with value = v });
             field (Lwd.pure @@ Field.submit (`P "Add row")) (fun t _v -> t);
           ])
  end in
  create
    (module Connect_form)
    (fun t ->
      Console.log [ "Form submitted:"; t ];
      match t with
      (* FIXME: validation already happened, it's redundant to have to match *)
      | { index = Ok index; id = Ok id; value = Ok value } ->
          Console.log [ "Form submitted:"; index; id; value ];
          Option.iter (fun content -> add_row content index id value) yjs_array
      | _ -> ())

let render_page =
  Lwd_table.map_reduce
    (fun _row data ->
      let open Lwd_infix in
      let div =
        let$* content = data in
        match content.data with
        | Table (v, content) ->
            let data_source = table_data_source content in
            let form = new_table_row_form v in
            let table = Virtual_bis.make ~ui_table:(table ()) data_source in
            Elwd.div
              ~at:Attrs.O.(v (`P (C "flex")))
              [
                `R form;
                `R (Elwd.div ~at:Attrs.O.(v (`P (C "table"))) [ `R table ]);
              ]
      in
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
      let v, _content = Data_table.make () in
      let page_array = Yjs.Doc.get_array yjs_doc "page_content" in
      Yjs.Array.push page_array [| `Map v |];
      Console.log [ "Create new table:" ])

let app =
  Elwd.div
    ~at:Attrs.O.(v (`P (C "flex")))
    [ `R new_table_form; `S (Lwd_seq.lift render_page) ]

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

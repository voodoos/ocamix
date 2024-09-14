open Brr
open Brr_lwd
open Brr_lwd_ui
open Brr_lwd_ui.Table

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
    - A Map storing column information columns uid -> name, type, etc
    - An Array storing entries in the table. Each element of the array is a Map from column uid to value. *)
module Data_table = struct
  open Yjs

  let init_content v =
    Console.log [ "NEW ARRAY" ];
    let content = Array.make () in
    Map.set v ~key:"content" (`Array content);
    content

  let make () =
    let v = Map.make () in
    let content = init_content v in
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
    t.index <- new_index;
    Console.debug
      [
        "[apply delta] New index:";
        Jv.of_array Jv.repr @@ Array.map Lwd_table.get @@ V.to_array t.index;
      ]
end

let rec lwd_of_yjs_map map =
  let open Yjs in
  let lwd_map = Lwd_map.make () in
  ignore
    (Map.fold_entries map ~f:(fun k v () -> Lwd_map.set lwd_map k v) ~init:());

  (* Observe changes *)
  let on_event (e : Map.Event.t) =
    Console.debug [ "On_change"; e ];
    let changes = Map.Event.keys_changes e in
    StringMap.iter
      (fun key -> function
        | {
            Map.Event.action = Add | Update;
            new_value =
              Some new_value (* TODO: might be a nested array or map *);
            old_value;
          } ->
            Console.debug
              [
                "Key:";
                key;
                "Action: add/update";
                "New value";
                new_value;
                "Old value:";
                old_value;
              ];
            Lwd_map.set lwd_map key new_value
        | { Map.Event.action = Delete; old_value; _ } ->
            Console.debug
              [ "Key:"; key; "Action: delete"; "Old value:"; old_value ]
        | _ -> assert false)
      changes
  in
  ignore @@ Map.observe map on_event;
  lwd_map

and lwd_of_yjs_array arr =
  let lwd_table = Indexed_table.make () in
  let f ~index value _array =
    let value =
      match value with
      | `Jv jv ->
          Console.debug [ "Value: jv"; jv; "index:"; index ];
          `Jv jv
      | `Map map ->
          Console.debug [ "Value: map"; map; "index:"; index ];
          `Map (lwd_of_yjs_map map)
      | `Array jv ->
          Console.debug [ "Value: array"; jv; "index:"; index ];
          `Array (lwd_of_yjs_array jv)
    in
    Indexed_table.append ~set:value lwd_table |> ignore
  in
  (* Load initial value (maybe we could also rely on the delta here) *)
  Yjs.Array.iter ~f arr;
  (* Observe changes *)
  let on_event (e : Yjs.Array.change Yjs.Event.t) =
    Console.debug [ "On_change"; e ];
    let delta = (Yjs.Event.changes e).delta in
    Console.debug [ ("Delta:", delta) ];
    Indexed_table.apply_delta ~map:lwd_of_yjs_value lwd_table delta
  in
  ignore @@ Yjs.Array.observe arr on_event;
  lwd_table

and lwd_of_yjs_value = function
  | `Map map -> `Map (lwd_of_yjs_map map)
  | `Jv jv -> `Jv jv
  | `Array jv -> `Array (lwd_of_yjs_array jv)

let lwd_of_yjs_page =
  let items = Yjs.Doc.get_array yjs_doc "" in
  lwd_of_yjs_array items

let content yjs_data_table =
  let open Lwd_infix in
  let$ content = Lwd_map.get yjs_data_table "content" in
  match content with
  | Some (`Array v) -> (Some v, lwd_of_yjs_array v)
  | _ -> (None, Indexed_table.make ())

let data_source (content : _ Indexed_table.t) =
  let reduce_row map =
    Lwd_table.map_reduce
      (fun _row { Lwd_map.key; value } ->
        let elt =
          let value =
            let open Lwd_infix in
            let$ value = Lwd.get value in
            Option.map
              (function
                | `Jv jv ->
                    Console.log [ key; ": jv"; jv ];
                    Lwd_seq.element @@ El.txt' (Jv.to_string jv)
                | `Map map ->
                    Console.log [ key; ": map"; map ];
                    Lwd_seq.element @@ El.txt' "array"
                | `Array jv ->
                    Console.log [ key; ": array"; jv ];
                    Lwd_seq.element @@ El.txt' "array")
              value
            |> Option.value ~default:Lwd_seq.empty
          in
          Elwd.div [ `S value ]
        in
        Lwd_seq.element elt)
      Lwd_seq.monoid map
  in
  Virtual_bis.
    {
      total_items = Lwd.pure 0;
      source_rows = content.table;
      render =
        (fun _ data ->
          match data with
          | `Jv jv ->
              Console.log [ "Value: jv"; jv ];
              Lwd.pure
                (Lwd_seq.element (Elwd.div [ `P (El.txt' (Jv.to_string jv)) ]))
          | `Map map ->
              Console.log [ "Value: map"; map ];
              reduce_row map.Lwd_map.table
          | `Array jv ->
              Console.log [ "Value: array"; jv ];
              Lwd.pure (Lwd_seq.element (Elwd.div [ `P (El.txt' "array") ])));
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
  let row = Yjs.Map.make () in
  Yjs.Map.set row ~key:"0" (`Jv (Jv.of_string id));
  Yjs.Map.set row ~key:"1" (`Jv (Jv.of_string v));
  Console.debug [ "Inserting row"; v ];
  Yjs.Array.insert content i [| `Map row |]

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
    (fun _row -> function
      | `Map data_table ->
          let open Lwd_infix in
          let div =
            let$* v, content = content data_table in
            let data_source = data_source content in
            let form = new_table_row_form v in
            let table = Virtual_bis.make ~ui_table:(table ()) data_source in
            Elwd.div
              ~at:Attrs.O.(v (`P (C "flex")))
              [
                `R form;
                `R (Elwd.div ~at:Attrs.O.(v (`P (C "table"))) [ `R table ]);
              ]
          in
          Lwd_seq.element div
      | _ -> assert false)
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
      let page_array = Yjs.Doc.get_array yjs_doc "" in
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

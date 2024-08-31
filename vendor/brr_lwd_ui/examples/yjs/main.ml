open Brr
open Brr_lwd
open Brr_lwd_ui
open Brr_lwd_ui.Table

let yjs_doc = Yjs.Doc.make ()

(* A data table is stored using multiple YJS shared types:
    - A Map storing general metadata
    - A Map storing column information columns uid -> name, type, etc
    - An Array storing entries in the table. Each element of the array is a Map from column uid to value. *)
module Data_table = struct
  open Yjs

  let v = Doc.get_map yjs_doc "data_table"
  let columns = Map.make ()
  let content = Array.make ()

  let () =
    Map.set columns "0"
      (`Jv
        (Jv.obj
           [|
             ("name", Jv.of_string "Column 1"); ("type", Jv.of_string "string");
           |]));
    Map.set v "columns" (`Map columns);
    Map.set v "content" (`Array content)
end

module Lwd_map = struct
  type 'a binding = { key : string; value : 'a }

  type 'a t = {
    table : 'a binding Lwd_table.t;
    map : (string, 'a binding Lwd_table.row) Hashtbl.t;
  }

  let lwd_table t = t.table

  let make ?(size = 64) () =
    let table = Lwd_table.make () in
    let map = Hashtbl.create size in
    { table; map }

  let set t key value =
    match Hashtbl.find_opt t.map key with
    | Some row -> Lwd_table.set row { key; value }
    | None ->
        let row = Lwd_table.append ~set:{ key; value } t.table in
        Hashtbl.replace t.map key row

  let get t k =
    Option.bind (Hashtbl.find_opt t.map k) Lwd_table.get
    |> Option.map (fun b -> b.value)

  let delete t k =
    match Hashtbl.find_opt t.map k with
    | None -> ()
    | Some row ->
        Lwd_table.remove row;
        Hashtbl.remove t.map k
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
          Console.debug [ "[apply delta] Retain"; i ];
          blit_append old_index !cursor i new_index;
          cursor := !cursor + i
      | Yjs.Array.Delete i ->
          Console.debug [ "[apply delta] Delete"; i ];
          let last = V.get_last new_index in
          for _i = 1 to i do
            (* We remove the [i] next rows *)
            match Lwd_table.next last with
            | None -> assert false
            | Some row -> Lwd_table.remove last
          done;
          cursor := !cursor + i
      | Insert a ->
          Console.debug [ "[apply delta] Insert"; a ];
          (* Three cases:
              1. Insertion at the beginning of an empty table
              2. Insertion at the beginning, before the first row
              3. Insertion after another row *)
          if V.length new_index = 0 then
            match first t with
            | None ->
                (* Case 1: the table is empty *)
                Console.debug [ "Insert in empty table" ];
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
                      Console.debug [ "Insert before first element"; set ];
                      Lwd_table.before first_row ~set)
                    a
                in
                V.append_array new_index rows
          else (
            (* Case 3 *)
            Console.debug [ "Insert after some element" ];
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
            V.append_array new_index rows)
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
    let changes = Map.Event.keys_changes e in
    StringMap.iter
      (fun key -> function
        | {
            Map.Event.action = Add | Update;
            new_value = Some new_value;
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
        | { Map.Event.action = Delete; old_value } ->
            Console.debug
              [ "Key:"; key; "Action: delete"; "Old value:"; old_value ])
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
          `Array jv
    in
    Indexed_table.append ~set:value lwd_table |> ignore
  in
  (* Load initial value (maybe we could also rely on the delta here) *)
  Yjs.Array.iter ~f arr;
  (* Observe changes *)
  let on_event (e : Yjs.Array.change Yjs.Event.t) =
    let delta = (Yjs.Event.changes e).delta in
    Console.debug [ ("Delta:", delta) ];
    Indexed_table.apply_delta ~map:lwd_of_yjs_value lwd_table delta
  in
  ignore @@ Yjs.Array.observe arr on_event;
  lwd_table

and lwd_of_yjs_value = function
  | `Map map -> `Map (lwd_of_yjs_map map)
  | `Jv jv -> `Jv jv
  | `Array jv -> `Array jv

let yjs_table = lwd_of_yjs_array Data_table.content

let () =
  Yjs.Array.insert Data_table.content 0 [| `Jv (Jv.of_string "00") |];
  Yjs.Array.insert Data_table.content 1 [| `Jv (Jv.of_string "11") |];
  Yjs.Array.insert Data_table.content 1 [| `Jv (Jv.of_string "12") |];
  Yjs.Array.insert Data_table.content 0 [| `Jv (Jv.of_string "01") |]

let row = Yjs.Map.make ()
let _ = Yjs.Array.insert Data_table.content 2 [| `Map row |]
let _ = Yjs.Map.set row ~key:"TOTORO" (`Jv (Jv.of_string "TAA2"))
let _ = Yjs.Map.set row ~key:"TOTORO" (`Jv (Jv.of_string "TAA3"))

let data =
  {
    Virtual.total_items = Lwd.pure 93_3;
    fetch = Lwd.pure (fun i -> Fut.ok (Array.map (fun i -> Some (i - 1)) i));
    render =
      Lwd.pure (fun i data ->
          [ `P (El.txt' (string_of_int i)); `P (El.txt' (string_of_int data)) ]);
  }

let reduce_row map =
  Lwd_table.map_reduce
    (fun _row v ->
      let elt =
        match v with
        | { Lwd_map.key; value = `Jv jv } ->
            Console.log [ key; ": jv"; jv ];
            Elwd.div [ `P (El.txt' (Jv.to_string jv)) ]
        | { key; value = `Map map } ->
            Console.log [ key; ": map"; map ];
            Elwd.div [ `P (El.txt' "array") ]
        | { key; value = `Array jv } ->
            Console.log [ key; ": array"; jv ];
            Elwd.div [ `P (El.txt' "array") ]
      in
      Lwd_seq.element elt)
    Lwd_seq.monoid map

let reduce_table tbl =
  Lwd_table.map_reduce
    (fun _row v ->
      let elt =
        match v with
        | `Jv jv ->
            Console.log [ "Value: jv"; jv ];
            Elwd.div [ `P (El.txt' (Jv.to_string jv)) ]
        | `Map map ->
            Console.log [ "Value: map"; map ];
            Elwd.div [ `S (Lwd_seq.lift @@ reduce_row map.Lwd_map.table) ]
        | `Array jv ->
            Console.log [ "Value: array"; jv ];
            Elwd.div [ `P (El.txt' "array") ]
      in
      Lwd_seq.element elt)
    Lwd_seq.monoid tbl

let app =
  let table =
    {
      columns =
        [|
          Columns.v "a" "5em" [ `P (El.txt' "id") ];
          Columns.v "a" "1fr" [ `P (El.txt' "minus1") ];
        |];
    }
  in
  let table = { table; row_height = Em 5. } in
  let table = Virtual.make ~ui_table:table data in
  let yjs_table = reduce_table yjs_table.table in
  Elwd.div
    ~at:Attrs.O.(v (`P (C "flex")))
    [
      `P (El.div [ El.txt' "options" ]);
      (* `R (Elwd.div ~at:Attrs.O.(v (`P (C "table"))) [ `R table ]);
       *)
      `R
        (Elwd.div
           ~at:Attrs.O.(v (`P (C "table")))
           [ `S (Lwd_seq.lift yjs_table) ]);
    ]

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

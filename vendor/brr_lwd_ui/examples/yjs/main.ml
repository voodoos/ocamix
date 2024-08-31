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
    let row0 = Map.make () in
    Map.set row0 "0" (`Jv (Jv.of_string "toto1"));
    Array.insert content 0 [| `Map row0 |];
    Map.set v "columns" (`Map columns);
    Map.set v "content" (`Array content)
end

module Indexed_table = struct
  module V = Hector.Poly

  type 'a t = { table : 'a Lwd_table.t; mutable index : 'a Lwd_table.row V.t }

  let make ?(size = 64) () =
    let table = Lwd_table.make () in
    let index = V.create () in
    { table; index }

  let first t = try Some (V.get t.index 0) with Invalid_argument _ -> None

  let append ?set t =
    let row = Lwd_table.append ?set t.table in
    V.add_last t.index row

  let apply_delta (t : Yjs.Array.value t) delta =
    let cursor = ref 0 in
    let old_index = t.index in
    let new_index = V.create () in
    let apply_one = function
      | Yjs.Array.Retain i ->
          Console.debug [ "[apply delta] Retain"; i ];
          let kept = V.sub old_index !cursor i in
          V.append new_index kept;
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
                  (fun set -> append ~set { t with index = new_index })
                  a
            | Some first_row ->
                (* Case 2 *)
                let rows =
                  Array.map
                    (fun set ->
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
                (fun set ->
                  let row = Lwd_table.after !last ~set in
                  last := row;
                  row)
                a
            in
            V.append_array new_index rows)
    in
    Array.iter apply_one delta;
    (* Todo, we could rezise and blit *)
    V.append new_index (V.sub old_index !cursor (V.length old_index - !cursor));
    t.index <- new_index;
    Console.debug
      [
        "[apply delta] New index:";
        Jv.of_array Jv.repr @@ Array.map Lwd_table.get @@ V.to_array t.index;
      ]
end

let lwd_of_yjs_array arr =
  let lwd_table = Indexed_table.make () in
  let f ~index value _array =
    let () =
      match value with
      | `Jv jv -> Console.log [ "Value: jv"; jv; "index:"; index ]
      | `Map jv -> Console.log [ "Value: map"; jv; "index:"; index ]
      | `Array jv -> Console.log [ "Value: array"; jv; "index:"; index ]
    in
    Indexed_table.append ~set:value lwd_table |> ignore
  in
  (* Load initial value (maybe we could also rely on the delta here) *)
  Yjs.Array.iter ~f arr;
  (* Observe changes *)
  let on_event (e : Yjs.Array.change Yjs.Event.t) =
    let delta = (Yjs.Event.changes e).delta in
    Console.log [ ("Delta:", delta) ];
    Indexed_table.apply_delta lwd_table delta
  in
  ignore @@ Yjs.Array.observe arr on_event;
  lwd_table

let yjs_table = lwd_of_yjs_array Data_table.content

let () =
  let row1 = Yjs.Map.make () in
  Yjs.Array.insert Data_table.content 1 [| `Jv (Jv.of_string "11") |];
  Yjs.Array.insert Data_table.content 1 [| `Jv (Jv.of_string "12") |];
  Yjs.Array.insert Data_table.content 0 [| `Jv (Jv.of_string "00") |];
  Yjs.Array.insert Data_table.content 0 [| `Jv (Jv.of_string "01") |]

let data =
  {
    Virtual.total_items = Lwd.pure 93_3;
    fetch = Lwd.pure (fun i -> Fut.ok (Array.map (fun i -> Some (i - 1)) i));
    render =
      Lwd.pure (fun i data ->
          [ `P (El.txt' (string_of_int i)); `P (El.txt' (string_of_int data)) ]);
  }

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
  let yjs_table =
    Lwd_table.(
      map_reduce
        (fun _row _v ->
          let txt =
            match _v with
            | `Jv jv ->
                Console.log [ "Value: jv"; jv ];
                Jv.to_string jv
            | `Map jv ->
                Console.log [ "Value: map"; jv ];
                "map"
            | `Array jv ->
                Console.log [ "Value: array"; jv ];
                "array"
          in
          Lwd_seq.element @@ Elwd.div [ `P (El.txt' txt) ])
        Lwd_seq.monoid yjs_table.table)
  in
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

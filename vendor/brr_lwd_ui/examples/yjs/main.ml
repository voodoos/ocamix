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
  type 'a t = {
    size : int Lwd.var;
    table : 'a Lwd_table.t;
    index : 'a Lwd_table.row Dynarray.t;
  }

  let make ?(size = 64) () =
    let table = Lwd_table.make () in
    let index = Dynarray.create () in
    { size = Lwd.var 0; table; index }

  let append ?set t =
    let row = Lwd_table.append ?set t.table in
    let size = Lwd.peek t.size in
    Dynarray.add_last t.index row;
    Lwd.set t.size (size + 1)
end

let replay_arr arr delta =
  let apply_one = function Yjs.Array.Retain i -> () in
  Array.iter delta

let lwd_of_yjs_array arr =
  let lwd_table = Indexed_table.make () in
  let f ~index value _array =
    let () =
      match value with
      | `Jv jv -> Console.log [ "Value: jv"; jv; "index:"; index ]
      | `Map jv -> Console.log [ "Value: map"; jv; "index:"; index ]
      | `Array jv -> Console.log [ "Value: array"; jv; "index:"; index ]
    in
    Indexed_table.append ~set:"toto" lwd_table |> ignore
  in
  (* Load initial value (maybe we could also rely on the delta here) *)
  Yjs.Array.iter ~f arr;
  (* Observe changes *)
  let on_event (e : Yjs.Array.change Yjs.Event.t) =
    let delta = (Yjs.Event.changes e).delta in
    Console.log [ ("Delta:", delta) ];

    ()
  in
  ignore @@ Yjs.Array.observe arr on_event;
  lwd_table

let _ = lwd_of_yjs_array Data_table.content

let () =
  let row1 = Yjs.Map.make () in
  Yjs.Map.set row1 "0" (`Jv (Jv.of_string "toto1"));
  Yjs.Array.insert Data_table.content 1 [| `Map row1 |]

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
  Elwd.div
    ~at:Attrs.O.(v (`P (C "flex")))
    [
      `P (El.div [ El.txt' "options" ]);
      `R (Elwd.div ~at:Attrs.O.(v (`P (C "table"))) [ `R table ]);
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

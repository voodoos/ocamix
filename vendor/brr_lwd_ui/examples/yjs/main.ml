open Brr
open Brr_lwd
open Brr_lwd_ui
open Brr_lwd_ui.Table

let yjs_doc = Yjs.Doc.make ()
let yjs_array = Yjs.Doc.get_array yjs_doc "test_array"

let _ =
  Yjs.Array.observe yjs_array (fun e ->
      Console.log [ "Event:"; e ];
      Console.log [ "Delta:"; (Yjs.Event.changes e).delta ])

let _ = Yjs.Array.insert yjs_array 0 [| Jv.of_int 1 |];;

Console.log [ yjs_doc; yjs_array ]

let () =
  let open Yjs in
  let yjs_map = Yjs.Doc.get_map yjs_doc "test_map" in
  let () = Map.set yjs_map ~key:"toto" (Jv.of_string "tata") in
  Console.log [ Map.entries yjs_map |> Jv.It.next ]

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

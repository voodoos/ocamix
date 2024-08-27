open Brr
open Brr_lwd
open Brr_lwd_ui
open Brr_lwd_ui.Table

let data =
  {
    Virtual.total_items = Lwd.pure 93_300;
    fetch =
      Lwd.pure (fun i ->
          (* Console.log [ "Loading"; Jv.of_array Jv.of_int i ]; *)
          Fut.ok (Array.map (fun i -> Some (i * i)) i));
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
          Columns.v "a" "1fr" [ `P (El.txt' "square") ];
        |];
    }
  in
  let table = { table; row_height = Em 5. } in
  let table = Virtual.make ~ui_table:table data in
  Elwd.div
    ~at:Attrs.O.(v (`P (A (At.style (Jstr.v "height:50vh")))))
    [ `R table ]

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

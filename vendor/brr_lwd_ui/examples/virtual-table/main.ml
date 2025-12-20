open Brrer
open Brr
open Brr_lwd
open Brr_lwd_ui
open Brr_lwd_ui.Table

let _renderer =
  Table.Virtual.with_placeholder_or_error @@ fun i data ->
  Lwd.return
    (Lwd_seq.of_list
       [
         Lwd.return (El.txt' (string_of_int i));
         Lwd.return (El.txt' (string_of_int data));
       ])

let _data =
  Data_source.Lazy
    {
      total_items = Lwd.pure 300;
      fetch =
        Lwd.pure (fun i ->
            (* Console.log [ "Loading"; Jv.of_array Jv.of_int i ]; *)
            Array.map (fun i -> Fut.ok (i * i)) i);
    }

let app =
  let columns =
    Lwd.pure
    @@ Lwd_seq.of_array
         [|
           Columns.v "a" (Em 5.) [ `P (El.txt' "id") ];
           Columns.v "a" (Fr 1.)
             [ `P (El.txt' "val") ]
             ~on_sort:(Set (Sort.int ()));
           Columns.v "a" (Fr 1.)
             [ `P (El.txt' "square") ]
             ~on_sort:(Set (Sort.int ~proj:(( * ) 2) ()));
         |]
  in
  let layout =
    make_fixed_row_height columns ~row_height:(Css_length.Em 5.) ()
  in
  (* let scroll_target = Lwd.var 0 in *)
  let data = Lwd_table.make () in
  let () =
    for _ = 0 to 1_000 do
      Lwd_table.append' data (Random.int 1000)
    done
  in
  let renderer row_index _row value =
    let s =
      Lwd.map (Lwd.get row_index) ~f:(fun i ->
          (*render index data*)
          (* todo*)
          let i = string_of_int i in
          El.txt' i)
    in
    let s = Elwd.div [ `R s ] in
    Lwd.return
      (Lwd_seq.of_list
         [
           s;
           Elwd.div [ `P (El.txt' (value |> string_of_int)) ];
           Elwd.div [ `P (El.txt' (value * value |> string_of_int)) ];
         ])
  in
  let table = Virtual.make' ~layout data renderer in
  let add_first =
    let ev =
      [ `P (Elwd.handler Ev.click (fun _ -> Lwd_table.prepend' data (-1))) ]
    in
    Elwd.button ~ev [ `P (El.txt' "add_first") ]
  in
  let add_2 =
    let ev =
      [
        `P
          (Elwd.handler Ev.click (fun _ ->
               ignore
               @@ (Lwd_table.first data |> Option.get |> Lwd_table.after ~set:2)));
      ]
    in
    Elwd.button ~ev [ `P (El.txt' "add_2") ]
  in
  let rm_2 =
    let ev =
      [
        `P
          (Elwd.handler Ev.click (fun _ ->
               ignore
               @@ (Lwd_table.first data |> Option.get |> Lwd_table.next
                 |> Option.get |> Lwd_table.remove)));
      ]
    in
    Elwd.button ~ev [ `P (El.txt' "rm_2") ]
  in
  let add_last =
    let ev =
      [ `P (Elwd.handler Ev.click (fun _ -> Lwd_table.append' data 100)) ]
    in
    Elwd.button ~ev [ `P (El.txt' "add_last") ]
  in
  Elwd.div
    ~at:Attrs.O.(v (`P (C "flex")))
    [
      `R (Elwd.div [ `R add_first; `R add_2; `R rm_2; `R add_last ]);
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

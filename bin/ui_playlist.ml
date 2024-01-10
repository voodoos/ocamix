open Import
open Brrer
open Brrer.Brr
open! Brr_lwd_ui
open! Brr_lwd
module Api = DS.Api

(** Part that should move to Brr_lwd_ui when ready *)
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

type 'a row_data = { index : int; content : 'a option }

let lazy_table (type data) ~columns ~total
    ~(fetch : int -> (data, _) Fut.result)
    ?(placeholder : int -> Elwd.t Elwd.col = fun _ -> [])
    ~(render : int -> data -> Elwd.t Elwd.col) () =
  ignore placeholder;
  let table : data row_data Lwd_table.t = Lwd_table.make () in
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
       let+ data = fetch row_data.index in
       Lwd_table.set row { row_data with content = Some data })
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
    (* Console.log [ "add"; i ]; *)
    if Int_uniqueue.add i unload_queue then (
      load i;
      cleanup ())
  in
  let num_rows = Lwd.var 0 in
  let _ =
    let open Fut.Result_syntax in
    let+ total = total in
    if not (Lwd.peek num_rows = total) then Lwd.set num_rows total;
    for i = 0 to total - 1 do
      let _uuid = new_uuid_v4 () |> Uuidm.to_string in
      let set = { index = i; content = None } in
      Hashtbl.add row_index i @@ Lwd_table.append ~set table;
      if i < 20 then add i (* preload the first items *)
    done
  in
  let render _ { content; index } =
    let at = Attrs.(classes [ "row" ] |> to_at) in
    let style =
      `P (At.style (Jstr.v @@ Printf.sprintf "top: %ipx" ((index + 1) * 70)))
    in
    match content with
    | Some data ->
        Lwd_seq.element @@ Elwd.div ~at:(style :: at) (render index data)
    | None -> Lwd_seq.empty
  in

  let table_body = Lwd_table.map_reduce render Lwd_seq.monoid table in
  let scroll_handler =
    let last_scroll_y = ref 0. in
    let update div =
      let height elt =
        let jv = El.to_jv elt in
        Jv.get jv "clientHeight" |> Jv.to_int
      in

      let children = El.children div in
      let scroll_y = El.scroll_y div in
      let direction = if scroll_y >. !last_scroll_y then `Down else `Up in
      let () = last_scroll_y := scroll_y in
      let total_height = height div in
      let num_rows = Lwd.peek num_rows in
      (* todo: fixed height should be specified elsewhere *)
      let _header_height = height @@ List.hd children in
      let _row_height = height @@ List.hd @@ List.tl children in
      let header_height = 70 in
      let row_height = 70 in
      let number_of_visible_rows = (total_height / row_height) + 1 in
      let bleeding = number_of_visible_rows in
      let scroll_y = scroll_y -. float_of_int header_height in
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
    let last_update = ref 0. in
    let ticker = ref (-1) in
    let reset_ticker div =
      let now = Performance.now_ms G.performance in
      if !ticker >= 0 then G.stop_timer !ticker;
      ticker := G.set_timeout ~ms:25 (fun () -> update div);
      if now -. !last_update >. 25. then (
        last_update := now;
        update div)
    in
    fun div -> reset_ticker div
  in
  let table_header = Table.Columns.to_header columns in
  let table_footer =
    Lwd.get num_rows
    |> Lwd.map ~f:(fun num_rows ->
           let style =
             At.style
               (Jstr.v @@ Printf.sprintf "top: %ipx" ((num_rows + 1) * 70))
           in
           El.div ~at:[ style ] [ El.txt' "foot" ])
  in
  let at = Attrs.to_at ~id:"lazy_tbl" @@ Attrs.classes [ "lazy-table" ] in
  let elt =
    Elwd.div
      ~ev:
        [
          `P
            (Elwd.handler Ev.scroll (fun ev ->
                 let div = Ev.target ev |> Ev.target_to_jv |> El.of_jv in
                 scroll_handler div));
        ]
      ~at
      [ `R table_header; `S (Lwd_seq.lift table_body); `R table_footer ]
  in
  let set = ref false in
  Lwd.app
    (Lwd.return (fun e ->
         (* TODO this should not always retrigger*)
         if not !set then (
           set := true;
           Console.log [ "SET GRID TEMPLATE ROWS" ];
           Table.Columns.set_style columns e)
         else e))
    elt

(** Application part *)

(** Columns declaration *)
let columns =
  Table.Columns.
    [|
      v "Order" "5rem" @@ [ `P (El.txt' "#") ];
      v "Cover" "5rem" @@ [ `P (El.txt' "Cover") ];
      v "Title" "1fr" @@ [ `P (El.txt' "Title") ];
    |]

let make ~reset_playlist ~servers ~fetch _ view =
  let total =
    Fut.map (Result.map (fun { Db.View.item_count; _ } -> item_count)) view
  in
  let fetch i =
    let open Fut.Result_syntax in
    let* view = view in
    fetch view i
  in
  let img_url server_id item_id =
    let server : DS.connexion = List.assq server_id servers in
    Printf.sprintf "%s/Items/%s/Images/Primary?width=50" server.base_url item_id
  in
  let render start_index
      { Db.Stores.Items.item = { Api.Item.name; album_id; server_id; _ }; _ } =
    let play_from _ =
      ignore
        (let open Fut.Result_syntax in
         let+ view = view in
         reset_playlist { view with start_offset = start_index })
    in
    let play_on_click = Elwd.handler Ev.click play_from in
    [
      `P (El.div [ El.txt' (string_of_int (start_index + 1)) ]);
      `R
        (Elwd.div
           ~ev:[ `P play_on_click ]
           [
             `P
               (El.img
                  ~at:
                    [
                      At.src (Jstr.v @@ img_url server_id album_id); At.width 50;
                    ]
                  ());
           ]);
      `P (El.div [ El.span [ El.txt' name ] ]);
    ]
  in
  let placeholder _i = [] in
  lazy_table ~columns ~total ~fetch ~render ~placeholder ()

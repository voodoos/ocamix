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
  ignore fetch;
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

    if Int_uniqueue.add i unload_queue then (
      load i;
      cleanup ())
  in

  let _ =
    let open Fut.Result_syntax in
    let+ total = total in
    for i = 1 to total do
      let _uuid = new_uuid_v4 () |> Uuidm.to_string in
      let set = { index = i; content = None } in
      Hashtbl.add row_index i @@ Lwd_table.append ~set table;
      if i < 50 then add i
    done
  in
  let render _ { content; index } =
    let at = Attrs.(classes [ "row" ] |> to_at) in
    let elt =
      match content with
      | Some data ->
          (* todo: this won't make the row reactive *)
          Elwd.div ~at (render index data)
      | None -> Elwd.div (placeholder index)
    in
    Lwd_seq.element elt
  in
  let table_body = Lwd_table.map_reduce render Lwd_seq.monoid table in
  let scroll_handler =
    let last_scroll_y = ref 0. in
    fun div ->
      (* todo: debounce *)
      (* todo: adjust bleeding with speed *)
      let height elt =
        let jv = El.to_jv elt in
        Jv.get jv "clientHeight" |> Jv.to_int
      in

      let children = El.children div in
      let scroll_y = El.scroll_y div in
      let direction = if scroll_y >. !last_scroll_y then `Down else `Up in
      let () = last_scroll_y := scroll_y in
      let total_height = height div in
      let num_rows = List.length children in
      let header_height = height @@ List.hd children in
      let row_height = height @@ List.hd @@ List.tl children in
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
  let table_header = Table.Columns.to_header columns in
  let at = Attrs.to_at ~id:"lazy_tbl" @@ Attrs.classes [ "lazy-table" ] in
  let elt =
    Lwd.map ~f:(Table.Columns.set_style columns)
    @@ Elwd.div
         ~ev:
           [
             `P
               (Elwd.handler Ev.scroll (fun ev ->
                    let div = Ev.target ev |> Ev.target_to_jv |> El.of_jv in
                    scroll_handler div));
           ]
         ~at
         [ `R table_header; `S (Lwd_seq.lift table_body) ]
  in
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

let make ~servers ~fetch view =
  let total =
    Fut.map (Result.map (fun { Db.View.item_count; _ } -> item_count)) view
  in
  let fetch i =
    let open Fut.Result_syntax in
    let* view = view in
    fetch view i
  in
  let audio_url server_id item_id =
    let server : DS.connexion = List.assq server_id servers in
    Printf.sprintf
      "%s/Audio/%s/universal?api_key=%s&MaxStreamingBitrate=178723404&Container=opus,webm|opus,mp3,aac,m4a|aac,m4b|aac,flac,webma,webm|webma,wav,ogg&TranscodingContainer=ts&TranscodingProtocol=hls&AudioCodec=aac"
      server.base_url item_id server.auth_response.access_token
  in
  let img_url server_id item_id =
    let server : DS.connexion = List.assq server_id servers in
    Printf.sprintf "%s/Items/%s/Images/Primary" server.base_url item_id
  in
  let render i
      {
        Db.Stores.Items.item = { Api.Item.name; album_id; id; server_id; _ };
        _;
      } =
    let play () = Lwd.set Player.now_playing (Some (audio_url server_id id)) in
    let play_on_click = Elwd.handler Ev.click (fun _ -> play ()) in
    [
      `P (El.div [ El.txt' (string_of_int i) ]);
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
  lazy_table ~columns ~total ~fetch ~render ()

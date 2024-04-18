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

  let clear t =
    let new_queue = create () in
    t.queue <- new_queue.queue;
    t.uniq <- new_queue.uniq
end

module Int_uniqueue = Uniqueue (Int)

type 'a row_data = {
  index : int;
  content : 'a option;
  render : int -> 'a -> Elwd.t Elwd.col;
}

type ('data, 'error) data_source = {
  total_items : (int, 'error) Fut.result;
  fetch : int array -> ('data option array, 'error) Fut.result;
  render : int -> 'data -> Elwd.t Elwd.col;
}

let lazy_table (type data) ~(ui_table : Table.fixed_row_height)
    ?(placeholder : int -> Elwd.t Elwd.col = fun _ -> [])
    (data_source : (data, _) data_source Lwd.t) =
  ignore placeholder;
  let row_size = ui_table.row_height |> Utils.Unit.to_string in
  let height_n n = Printf.sprintf "height: calc(%s * %i);" row_size n in
  let height = Printf.sprintf "height: %s !important;" row_size in
  let table : data row_data Lwd_table.t = Lwd_table.make () in
  (* The [rows] table is used to relate divs to the table's rows in the
     observer's callback *)
  let row_index = Hashtbl.create 2048 in
  let unload_queue = Int_uniqueue.create () in

  let add ~fetch ?(max_items = 200) indexes =
    let unload i =
      let open Option.Infix in
      (let* row = Hashtbl.get row_index i in
       let+ row_data = Lwd_table.get row in
       Lwd_table.set row { row_data with content = None })
      |> ignore
    in
    let load indexes =
      (let open Fut.Result_syntax in
       let+ (data : data option array) = fetch indexes in
       Array.iter2 indexes data ~f:(fun i data ->
           (let open Option.Infix in
            let* row = Hashtbl.get row_index i in
            let+ row_data = Lwd_table.get row in
            let data =
              match data with Some data -> data | _ -> raise Not_found
            in
            Lwd_table.set row { row_data with content = Some data })
           |> ignore))
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
    let to_load =
      List.filter indexes ~f:(fun i -> Int_uniqueue.add i unload_queue)
    in
    load (Array.of_list to_load);
    cleanup ()
  in
  let num_rows = Lwd.var 0 in

  let scroll_handler =
    Lwd.map data_source ~f:(fun { total_items; fetch; render } ->
        let add = add ~fetch in
        let scroll_handler =
          let last_scroll_y = ref 0. in
          let update div =
            let height elt =
              let jv = El.to_jv elt in
              Jv.get jv "offsetHeight" |> Jv.to_int
            in
            let children = El.children div in
            let scroll_y = El.scroll_y div in
            let direction = if scroll_y >. !last_scroll_y then `Down else `Up in
            let () = last_scroll_y := scroll_y in
            let total_height = height div |> float_of_int in
            let num_rows = Lwd.peek num_rows in
            let header_height = height @@ List.hd children in
            let first_row = List.hd @@ List.tl children in
            let row_height =
              Utils.Unit.to_px ~parent:first_row ui_table.row_height
            in
            let number_of_visible_rows =
              total_height /. row_height |> int_of_float
            in
            let bleeding = number_of_visible_rows in
            let scroll_y = scroll_y -. float_of_int header_height in
            let first_visible_row = int_of_float (scroll_y /. row_height) + 1 in
            let last_visible_row = first_visible_row + number_of_visible_rows in
            let first =
              let bleeding =
                match direction with `Up -> bleeding | _ -> bleeding / 2
              in
              first_visible_row - bleeding |> max 0
            in
            let last =
              let bleeding =
                match direction with `Down -> bleeding | _ -> bleeding / 2
              in
              last_visible_row + bleeding |> min num_rows
            in
            (* todo: We do way too much work and rebuild the queue each
               time... it's very ineficient *)
            let indexes = List.init (last - first) ~f:(fun i -> first + i) in
            add ~max_items:(4 * number_of_visible_rows) indexes
          in
          let last_update = ref 0. in
          let timeout = ref (-1) in
          let reset_ticker div =
            let debouncing_interval = 25 in
            (* We use [last_update] to have regular debounced updates and the
               [timeout] to ensure that the last scroll event is always taken into
               account even it it happens during the debouncing interval. *)
            let now = Performance.now_ms G.performance in
            if !timeout >= 0 then G.stop_timer !timeout;
            timeout :=
              G.set_timeout ~ms:debouncing_interval (fun () -> update div);
            if now -. !last_update >. float_of_int debouncing_interval then (
              last_update := now;
              update div)
          in
          fun div -> reset_ticker div
        in
        let _ =
          let open Fut.Result_syntax in
          let () = Int_uniqueue.clear unload_queue in
          let+ total = total_items in
          if not (Lwd.peek num_rows = total) then Lwd.set num_rows total;
          Lwd_table.clear table;
          for i = 0 to total - 1 do
            let set = { index = i; content = None; render } in
            Hashtbl.add row_index i @@ Lwd_table.append ~set table
          done;
          (* preload the first items *)
          add (List.init 100 ~f:Fun.id)
        in

        Elwd.handler Ev.scroll (fun ev ->
            let div = Ev.target ev |> Ev.target_to_jv |> El.of_jv in
            scroll_handler div))
  in
  let make_spacer n =
    let at = [ At.class' (Jstr.v "row_spacer") ] in
    let style = At.style (Jstr.v @@ height_n n) in
    El.div ~at:(style :: at) []
  in
  let render _ { content; index; render } =
    let at = Attrs.add At.Name.class' (`P "row") [] in
    let style = `P (At.style (Jstr.v height)) in
    match content with
    | Some data ->
        (0, Lwd_seq.element @@ Elwd.div ~at:(style :: at) (render index data), 0)
    | None -> (1, Lwd_seq.empty, 0)
  in
  let table_body =
    let rows =
      Lwd_table.map_reduce render
        ( (0, Lwd_seq.empty, 0),
          fun (n, s, m) (p, s', q) ->
            match (Lwd_seq.view s, Lwd_seq.view s') with
            | Empty, Empty ->
                (* Since s is empty it does not matter on which
                   "side" of it the spaces are accumulated. *)
                (n + m + p + q, s, 0)
            | Empty, _ -> (n + m + p, s', q)
            | _, Empty -> (n, s, m + p + q)
            | _, _ ->
                let s =
                  if m + p > 0 then
                    let spacer = Lwd.pure @@ make_spacer (m + p) in
                    Lwd_seq.(concat s @@ concat (element spacer) s')
                  else Lwd_seq.concat s s'
                in
                (n, s, q) )
        table
    in
    Lwd.map rows ~f:(fun (n, s, m) ->
        let result =
          if n > 0 then
            let first_spacer = Lwd.pure @@ make_spacer n in
            Lwd_seq.(concat (element first_spacer) s)
          else s
        in
        if m > 0 then
          let last_spacer = Lwd.pure @@ make_spacer m in
          Lwd_seq.(concat result (element last_spacer))
        else result)
  in
  let table_header = Table.header ui_table in
  let at = Attrs.to_at ~id:"lazy_tbl" @@ Attrs.classes [ "lazy-table" ] in
  let grid_style = Table.style ui_table in
  let s = At.style (Jstr.v @@ grid_style) in
  let at = `P s :: at in
  Elwd.div
    ~at:Attrs.(to_at @@ classes [ "lazy-table-wrapper" ])
    [
      `R
        (Elwd.div
           ~ev:[ `R scroll_handler ]
           ~at
           [ `R table_header; `S (Lwd_seq.lift table_body) ]);
    ]

(** Application part *)

(** Columns declaration *)
let columns () =
  Table.Columns.
    [|
      v "Order" "5rem" @@ [ `P (El.txt' "#") ];
      v "Cover" "5rem" @@ [ `P (El.txt' "Cover") ];
      v "Title" "1fr" @@ [ `P (El.txt' "Title") ];
    |]

let make ~reset_playlist ~fetch _ (view : (Db.View.t, 'a) Fut.result Lwd.t) =
  let img_url server_id item_id =
    Lwd.map (Lwd.get Servers.var) ~f:(fun servers ->
        let servers = Lwd_seq.to_list servers in
        let url =
          try
            let server : Servers.server = List.assq server_id servers in
            Printf.sprintf "%s/Items/%s/Images/Primary?width=50"
              server.connexion.base_url item_id
          with Not_found -> "error-globe-64.png"
        in
        At.src (Jstr.v url))
  in
  let render view start_index
      {
        Db.Stores.Items.item =
          { Api.Item.id; name; album_id; server_id; image_blur_hashes; _ };
        _;
      } =
    let play_from _ =
      ignore
        (let open Fut.Result_syntax in
         let+ (view : Db.View.t) = view in
         reset_playlist
           { view with start_offset = view.start_offset + start_index })
    in
    let play_on_click = Elwd.handler Ev.click play_from in
    let img_url =
      match (image_blur_hashes, album_id) with
      | { primary = None }, _ | _, None ->
          Lwd.return (At.src (Jstr.v "music-50.png"))
      | _, Some id -> img_url server_id id
    in
    let status =
      Lwd.map (Lwd.get Player.now_playing) ~f:(function
        | Some { item_id; _ } when String.equal item_id id -> El.txt' "|>"
        | Some _ | None -> El.txt' (string_of_int (start_index + 1)))
    in
    [
      `R (Elwd.div [ `R status ]);
      `R
        (Elwd.div
           ~ev:[ `P play_on_click ]
           [ `R (Elwd.img ~at:[ `R img_url; `P (At.width 50) ] ()) ]);
      `P (El.div [ El.span [ El.txt' name ] ]);
    ]
  in
  let placeholder _i = [] in
  let ui_table =
    { Table.table = { columns = columns () }; row_height = Em 4. }
  in
  let data_source =
    Lwd.map view ~f:(fun view ->
        let total_items = Fut.map (Result.map Db.View.item_count) view in
        let fetch i =
          let open Fut.Result_syntax in
          let* view = view in
          fetch view i
        in
        let render = render view in
        { total_items; fetch; render })
  in
  lazy_table ~ui_table ~placeholder data_source

(*
###########**#******#%%#===+++*###%###########*+##=###++++++++++++++++++++++++++
###########*####****%%##===========+#===-=======*#=###++++++++++++++++++++++++++
###########*#####***#%##======---==+#==---======*#=###++++++++++++++++++++++++++
########################====----==-=#=--========*#=*##%#+++++%%%++++++++++++++++
######################%#====-====--=#===========*#=*##%#++=+#**%%+++++++++++++++
#######################*=----------=#===========+#=+##+%#%*+%+%%%+++++++++++++++
###################+=--------==-----#====-==----+#=+##*+%#%%+##%%+++++++++++++++
#################+=-------------------------==-=+#=+###+++#*%+#+++++++++++++++++
################=---------=####=#=--#=++====--===#=+###*+++%%++%%+++++++++++++++
##############*======--==**######+=====+=++++++*##=+###*++++++++++++++++++++++++
##############=-----==+*+###*######=#==----=====+#=+###****++++++++IS+++++++++++
##############==-=-=*#+=#+%+#===#===#==--========#=+###***++++++++++++THIS++++++
##############=-#*#+*##=####*+#=++==#===========+#=+###*******+++A++++++++++++++
##############==###=############+#*+#============#++###*********++++++++++++++++
###############=*+#*###############=#============#++###************MONOID+?+++++
################==##########====##*=#=======#**==#++###*****************++++++++
#################==+##############==#=====###====#++###**********************+++
##################==###+#########+###*+==###=====#++##+++**********************+
##############+%%%%########+####*==+#==+##############*++***********************
##############+++++=+%#%#*###*#%%%%+#=+##########*###+#=##*#********************
##############*#==+====##===###%%%%%%%#############=#+######****++*==+**********
#####+=+###########=+==+====###*+#%%%%###########*++########==*+======++********
####################*=======#####+#%%*########*==#++######*================*****
############+#####+####===+=#######+########=====#++###+===================+**** *)

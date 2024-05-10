(** A virtual table that can handle large dataset.

    TODO: this is clearly over-engineered: the large lwd table that reduces to 
    rows and placeholders with a monoid is elegant bu does not scale well. It 
    might be possible to optimize it (especially the "uniqueue" LRU thingy), 
    but having too large a lwd_table is probably a hard limit. *)

open Std
open Brrer
open Brr
open Brr_lwd

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

(* The virtual table is a complex reactive component. Primarily, it reacts to
   changes of the [data_source] so that content in the table is properly
   refreshed when it does. Additionnaly it needs to react to multiple dom
   events, notably vertical resize of the container and scroll events, to ensure
   that the visible part of the talbe is always populated with rows. *)

let make (type data) ~(ui_table : Schema.fixed_row_height)
    ?(placeholder : int -> Elwd.t Elwd.col = fun _ -> [])
    ?(scroll_target : int Lwd.t option)
    (data_source : (data, _) data_source Lwd.t) =
  ignore placeholder;
  let row_size = ui_table.row_height |> Utils.Unit.to_string in
  let height_n n = Printf.sprintf "height: calc(%s * %i);" row_size n in
  let height = Printf.sprintf "height: %s !important;" row_size in
  let table : data row_data Lwd_table.t = Lwd_table.make () in
  (* The [rows] table is used to relate divs to the table's rows in the
     observer's callback *)
  let row_index : (int, data row_data Lwd_table.row) Hashtbl.t =
    Hashtbl.create 2048
  in
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
          try unload (Int_uniqueue.take unload_queue) with Queue.Empty -> ()
        done
    in
    let to_load =
      List.filter indexes ~f:(fun i -> Int_uniqueue.add i unload_queue)
    in
    load (Array.of_list to_load);
    cleanup ()
  in
  let table_height = Lwd.var None in
  let compute_visible_rows ~last_scroll_y div =
    let height elt =
      let jv = El.to_jv elt in
      Jv.get jv "offsetHeight" |> Jv.to_int
    in
    let scroll_y = El.scroll_y div in
    let direction = if scroll_y >. !last_scroll_y then `Down else `Up in
    let () = last_scroll_y := scroll_y in
    let visible_height = height div |> float_of_int in
    let row_height = Utils.Unit.to_px ~parent:div ui_table.row_height in
    let header_height = row_height in
    let number_of_visible_rows = visible_height /. row_height |> int_of_float in
    let bleeding = number_of_visible_rows in
    let scroll_y = scroll_y -. header_height in
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
      last_visible_row + bleeding
    in
    List.init (last - first) ~f:(fun i -> first + i)
  in
  let prepare ~total_items:total ~render =
    let () =
      (* Cleanup *)
      Lwd_table.clear table;
      Hashtbl.clear row_index;
      Int_uniqueue.clear unload_queue
    in
    for i = 0 to total - 1 do
      let set = { index = i; content = None; render } in
      Hashtbl.add row_index i @@ Lwd_table.append ~set table
    done
  in
  let populate_on_scroll =
    Lwd.map data_source ~f:(fun { total_items; fetch; render } ->
        let add = add ~fetch in
        let last_scroll_y = ref 0. in
        let update div =
          let visible_rows = compute_visible_rows ~last_scroll_y div in
          (* todo: We do way too much work and rebuild the queue each
             time... it's very ineficient *)
          add ~max_items:(4 * List.length visible_rows) visible_rows
        in
        let open Fut.Syntax in
        let+ total_items = total_items in
        match total_items with
        | Ok total_items ->
            prepare ~total_items ~render;
            update
        | _ -> ignore)
  in
  let scroll_handler =
    Lwd.map populate_on_scroll ~f:(fun update ->
        Elwd.handler Ev.scroll (fun ev ->
            let open Fut.Syntax in
            ignore
            @@
            let+ update = update in
            let scroll_handler =
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
            let div = Ev.target ev |> Ev.target_to_jv |> El.of_jv in
            scroll_handler div))
  in
  let () =
    let repopulate_deps =
      Lwd.map2 populate_on_scroll (Lwd.get table_height) ~f:(fun a b -> (a, b))
    in
    let root = Lwd.observe repopulate_deps in
    Lwd.set_on_invalidate root (fun _ ->
        match Lwd.quick_sample root with
        | update, Some (_h, div) -> Fut.await update (fun update -> update div)
        | _ -> ());
    Lwd.quick_sample root |> ignore
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
  let table_header = Schema.header ui_table in
  let observer =
    (* We observe the size of the table to re-populate if necessary *)
    Resize_observer.create ~callback:(fun entries _ ->
        let entry = List.hd entries in
        let div = Resize_observer.Entry.target entry in
        let rect = Resize_observer.Entry.content_rect entry in
        let height = Dom_rect_read_only.height rect in
        match Lwd.peek table_height with
        | Some (h, _) when h <> height ->
            Lwd.set table_height (Some (height, div))
        | None -> Lwd.set table_height (Some (height, div))
        | _ -> ())
  in
  let at = Attrs.to_at @@ Attrs.classes [ "lwdui-lazy-table" ] in
  let grid_style = Schema.style ui_table in
  let s = At.style (Jstr.v @@ grid_style) in
  let at = `P s :: at in
  let table =
    Elwd.div
      ~ev:[ `R scroll_handler ]
      ~at
      [ `R table_header; `S (Lwd_seq.lift table_body) ]
    |> Lwd.map ~f:(tee (Resize_observer.observe observer))
  in
  let at = Attrs.O.(v (`P (C "lwdui-lazy-table-wrapper"))) in
  match scroll_target with
  | Some scroll_target ->
      let scroll_target =
        Lwd.map2 table scroll_target ~f:(fun parent i ->
            let row_height =
              Int.of_float (Utils.Unit.to_px ~parent ui_table.row_height)
            in
            Some (Controlled_scroll.Pos (i * row_height)))
      in
      Controlled_scroll.make ~at ~scroll_target table
  | None -> Elwd.div ~at [ `R table ]

(** #######**#******#%%#===+++*###%###########*+##=###++++++++++++++++++++++++++
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
############+#####+####===+=#######+########=====#++###+===================+***)

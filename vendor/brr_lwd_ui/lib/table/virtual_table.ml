(** A virtual table that can handle large dataset.

    TODO: this is clearly over-engineered: the large lwd table that reduces to
    rows and placeholders with a monoid is elegant bu does not scale well. It
    might be possible to optimize it (especially the "uniqueue" LRU thingy), but
    having too large a lwd_table is probably a hard limit. *)

open Import
open Brrer
open Brr
open Brr_lwd
module FRef = Utils.Forward_ref

let logger = Logger.for_section "virtual table"

type 'a row_data = {
  index : int;
  content : 'a option;
  render : (int -> 'a -> Elwd.t Elwd.col) Lwd.t;
}

type ('data, 'error) data_source = {
  total_items : int Lwd.t;
  fetch : (int array -> ('data option array, 'error) Fut.result) Lwd.t;
  render : (int -> 'data -> Elwd.t Elwd.col) Lwd.t;
}

(* The virtual table is a complex reactive component. Primarily, it reacts to
   changes of the [data_source] so that content in the table is properly
   refreshed when it does. Additionnaly it needs to react to multiple dom
   events, notably vertical resize of the container and scroll events, to ensure
   that the visible part of the talbe is always populated with rows. *)
module Cache = FFCache.Make (Int)

let make (type data) ~(ui_table : Schema.fixed_row_height)
    ?(placeholder : int -> Elwd.t Elwd.col = fun _ -> [])
    ?(scroll_target : int Lwd.t option)
    ({ total_items; fetch; render } : (data, _) data_source) =
  ignore placeholder;

  let module State = struct
    (* The wrapper_div ref should be initialized with the correct element as
       soon as it is created. It is not reactive per se. *)
    let content_div : El.t Utils.Forward_ref.t = Utils.Forward_ref.make ()

    (* The wrapper_div ref should be initialized with the correct element as
       soon as it is created. It is not reactive per se. *)
    let wrapper_div : El.t Utils.Forward_ref.t = Utils.Forward_ref.make ()

    (* The height of the window is a reactive value that might change during
       execution when the browser is resized or other layout changes are made. *)
    let _window_height : int option Lwd.var = Lwd.var None
  end in
  let row_size = ui_table.row_height |> Utils.Unit.to_string in
  let height_n n = Printf.sprintf "height: calc(%s * %i);" row_size n in
  let height = Printf.sprintf "height: %s !important;" row_size in
  let table : data row_data Lwd_table.t = Lwd_table.make () in
  (* The [row_index] table is used to provide fast random access to the table's
     rows in the observer's callback *)
  let row_index : (int, data row_data Lwd_table.row) Hashtbl.t =
    Hashtbl.create 2048
  in
  let unload i =
    let open Option.Infix in
    (let* row = Hashtbl.get row_index i in
     let+ row_data = Lwd_table.get row in
     Lwd_table.set row { row_data with content = None })
    |> ignore
  in
  let new_cache () = Cache.create ~size:50 in
  (* The cache is some sort of LRU to keep live the content of recently seen
     rows *)
  let cache_ref = ref (new_cache ()) in
  let add ~fetch ?(max_items = 200) indexes =
    let cache = !cache_ref in
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
            (* todo: let users provide comparison *)
            if not (Equal.poly row_data.content @@ Some data) then
              Lwd_table.set row { row_data with content = Some data })
           |> ignore))
      |> ignore
    in
    let cache, to_load =
      List.fold_left ~init:(cache, []) indexes ~f:(fun (cache, acc) i ->
          ignore max_items (* cache is not configurable right now *);
          let cache, inserted = Cache.insert ~on_evict:unload cache i i in
          if inserted then (cache, i :: acc) else (cache, acc))
    in
    cache_ref := cache;
    match Array.of_list to_load with [||] -> () | to_load -> load to_load
  in
  let table_height = Lwd.var None in
  let compute_visible_rows ~last_scroll_y =
    let height elt =
      let jv = El.to_jv elt in
      Jv.get jv "offsetHeight" |> Jv.to_float
    in
    let div = FRef.get_exn State.wrapper_div in
    let scroll_y = El.scroll_y div in
    let direction = if scroll_y >. !last_scroll_y then `Down else `Up in
    let () = last_scroll_y := scroll_y in
    let visible_height = height div in
    let parent = Utils.Forward_ref.get_exn State.content_div in
    let row_height = Utils.Unit.to_px ~parent ui_table.row_height in
    logger.debug [ "Visible height:"; visible_height; "Row height"; row_height ];
    let number_of_visible_rows =
      Int.of_float (ceil (visible_height /. row_height))
    in
    let bleeding = number_of_visible_rows in
    let scroll_y = scroll_y in
    let first_visible_row = int_of_float (scroll_y /. row_height) in
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
    let () = cache_ref := new_cache () in
    let i = ref 0 in
    let current_row = ref (Lwd_table.first table) in
    while Option.is_some !current_row || !i <= total - 1 do
      match !current_row with
      | Some row ->
          if !i <= total - 1 then
            let () = Hashtbl.replace row_index !i row in
            Lwd_table.set row { index = !i; content = None; render }
          else Lwd_table.unset row;
          incr i;
          current_row := Lwd_table.next row
      | None ->
          if !i <= total - 1 then (
            let set = { index = !i; content = None; render } in
            let row = Lwd_table.append ~set table in
            Hashtbl.add row_index !i row;
            incr i;
            current_row := Lwd_table.next row)
    done
  in
  let populate_on_scroll =
    let last_scroll_y = ref 0. in
    let update =
      Lwd.map fetch ~f:(fun fetch () ->
          let visible_rows = compute_visible_rows ~last_scroll_y in
          (* todo: We do way too much work and rebuild the queue each
             time... it's very ineficient *)
          add ~fetch ~max_items:(4 * List.length visible_rows) visible_rows)
    in
    Lwd.map2 total_items update ~f:(fun total_items update ->
        prepare ~total_items ~render;
        update)
  in
  let () =
    let repopulate_deps = Lwd.pair populate_on_scroll (Lwd.get table_height) in
    let root = Lwd.observe repopulate_deps in
    Lwd.set_on_invalidate root (fun _ ->
        match Lwd.quick_sample root with
        | update, Some _h -> update ()
        | _ -> ());
    Lwd.quick_sample root |> ignore
  in
  let make_spacer n =
    let at = [ At.class' (Jstr.v "row_spacer") ] in
    let style = At.style (Jstr.v @@ height_n n) in
    El.div ~at:(style :: at) []
  in
  let render _row { content; index; render } =
    let at = Attrs.add At.Name.class' (`P "lwdui-virtual-table-row") [] in
    let style = `P (At.style (Jstr.v height)) in
    match content with
    | Some data ->
        let rendered_row =
          Lwd.map render ~f:(fun render ->
              Lwd_seq.of_list
                (List.map (render index data) ~f:(fun elt -> Elwd.div [ elt ])))
        in
        ( 0,
          Lwd_seq.element
          @@ Elwd.div ~at:(style :: at) [ `S (Lwd_seq.lift rendered_row) ],
          0 )
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
        let rect = Resize_observer.Entry.content_rect entry in
        let height = Dom_rect_read_only.height rect in
        match Lwd.peek table_height with
        | Some h when h <> height -> Lwd.set table_height (Some height)
        | None -> Lwd.set table_height (Some height)
        | _ -> ())
  in
  let rows =
    let at = Attrs.O.(v (`P (C "lwdui-lazy-table-content"))) in
    let on_create el = Utils.Forward_ref.set_exn State.content_div el in
    Elwd.div ~at ~on_create [ `S (Lwd_seq.lift table_body) ]
  in
  let wrapper =
    let at = Attrs.O.(v (`P (C "lwdui-lazy-table-content-wrapper"))) in
    let scroll_handler =
      Lwd.map populate_on_scroll ~f:(fun update ->
          (* We use [last_update] to have regular debounced updates and the
             [timeout] to ensure that the last scroll event is always taken into
             account even it it happens during the debouncing interval. *)
          let last_update = ref 0. in
          let timeout = ref (-1) in
          Elwd.handler Ev.scroll (fun _ev ->
              let debouncing_interval = 50 in
              let now = Performance.now_ms G.performance in
              if !timeout >= 0 then G.stop_timer !timeout;
              timeout :=
                G.set_timeout ~ms:debouncing_interval (fun () -> update ());
              if now -. !last_update >. float_of_int debouncing_interval then (
                last_update := now;
                update ())))
    in
    let ev = [ `R scroll_handler ] in
    let on_create el = Utils.Forward_ref.set_exn State.wrapper_div el in
    (match scroll_target with
    | Some scroll_target ->
        let scroll_target =
          Lwd.map scroll_target ~f:(fun i ->
              let row_height =
                let parent = Utils.Forward_ref.get_exn State.content_div in
                Int.of_float (Utils.Unit.to_px ~parent ui_table.row_height)
              in
              Some (Controlled_scroll.Pos (i * row_height)))
        in
        Controlled_scroll.make ~at ~ev ~scroll_target
          (Elwd.div ~at ~ev ~on_create [ `R rows ])
    | None -> Elwd.div ~at ~ev ~on_create [ `R rows ])
    |> Lwd.map ~f:(tee (fun el -> Resize_observer.observe observer el))
  in
  let table =
    let at = Attrs.to_at @@ Attrs.classes [ "lwdui-lazy-table" ] in
    let grid_style = Schema.style ui_table in
    let s = Lwd.map grid_style ~f:(fun s -> At.style (Jstr.v s)) in
    let at = `R s :: at in
    Elwd.div ~at [ `R table_header; `R wrapper ]
  in
  table

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

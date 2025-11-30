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

type ('data, 'error) row_renderer =
  int -> ('data, 'error) Fut.result -> El.t Lwd.t Lwd_seq.t Lwd.t

let default_placeholder =
 fun _ -> Lwd.return (Lwd_seq.element (Lwd.return (El.txt' "Loading")))

let default_error =
 fun _ -> Lwd.return (Lwd_seq.element (Lwd.return (El.txt' "Error")))

let with_placeholder_or_error ?(placeholder = default_placeholder)
    ?(error = default_error) render i data =
  let data = Utils.var_of_fut_opt data in
  Lwd.bind (Lwd.get data) ~f:(function
    | Some (Ok data) -> render i data
    | Some (Error err) -> error err
    | None -> placeholder i)

type ('data, 'error) row_data = {
  index : int;
  content : ('data, 'error) Fut.result option;
}

type ('data, 'error) data_source =
  | Lazy of {
      total_items : int Lwd.t;
      fetch : (int array -> ('data, 'error) Fut.result array) Lwd.t;
          (** Fetched indices are always contiguous. *)
    }

let data_source_of_random_access_table (t : 'a Random_access_table.t) =
  let open Random_access_table in
  let fetch =
    let error = Jv.Error.v (Jstr.of_string "Not found") in
    Lwd.map t.index ~f:(fun index indices ->
        (* TODO There is an opportunity for an optimisation here: [RAList.get]
               runs in [log n] but since the indicies should be consecutive we could
               only [get] the first one and then iterate with [Lwd_table.next] in
               constant time. *)
        Array.map
          ~f:(fun i ->
            RAList.get index i |> Option.to_result error |> Fut.return)
          indices)
  in
  Lazy { total_items = t.length; fetch }

(* The virtual table is a complex reactive component. Primarily, it reacts to
       changes of the [data_source] so that content in the table is properly
       refreshed when it does. Additionnaly it needs to react to multiple dom
       events, notably vertical resize of the container and scroll events, to ensure
       that the visible part of the talbe is always populated with rows. *)
module Cache = FFCache.Make (Int)

module Dom = struct
  type state = {
    layout : Layout.fixed_row_height;
    (* The content_div ref should be initialized with the correct element as
         soon as it is created. It is not reactive per se. *)
    content_div : El.t Utils.Forward_ref.t;
    (* The wrapper_div ref should be initialized with the correct element as
       soon as it is created. It is not reactive per se. *)
    wrapper_div : El.t Utils.Forward_ref.t;
    (* The height of the window is a reactive value that might change during
       execution when the browser is resized or other layout changes are made. *)
    window_height : int option Lwd.var;
    table_height : int option Lwd.var;
    mutable last_scroll_y : float;
  }

  let resize_observer state =
    (* We observe the size of the table to re-populate if necessary *)
    Resize_observer.create ~callback:(fun entries _ ->
        let entry = List.hd entries in
        let rect = Resize_observer.Entry.content_rect entry in
        let height = Dom_rect_read_only.height rect in
        match Lwd.peek state.table_height with
        | Some h when h <> height -> Lwd.set state.table_height (Some height)
        | None -> Lwd.set state.table_height (Some height)
        | _ -> ())

  let with_scroll_position ?at dom_state target_position el =
    let scroll_target =
      Lwd.map target_position ~f:(fun i ->
          let row_height =
            let parent = Utils.Forward_ref.get_exn dom_state.content_div in
            Int.of_float (Utils.Unit.to_px ~parent dom_state.layout.row_height)
          in
          Some (Controlled_scroll.Pos (i * row_height)))
    in
    Controlled_scroll.make ?at ~scroll_target el

  let make_wrapper dom_state ?(on_create = Fun.id) ?scroll_target scroll_handler
      rows =
    let at = Attrs.O.(v (`P (C "lwdui-lazy-table-content-wrapper"))) in

    let ev = [ `R scroll_handler ] in
    let observer = resize_observer dom_state in
    let on_create el =
      Utils.Forward_ref.set_exn dom_state.wrapper_div el;
      Resize_observer.observe observer el;
      on_create ()
    in
    let wrapper = Elwd.div ~at ~ev ~on_create [ `R rows ] in
    match scroll_target with
    | Some scroll_target -> with_scroll_position dom_state scroll_target wrapper
    | None -> wrapper

  let make_table layout content =
    let table_header = Layout.header layout in
    let table_status = Layout.status layout in
    let at = Attrs.to_at @@ Attrs.classes [ "lwdui-lazy-table" ] in
    let grid_style = Layout.style layout in
    let s = Lwd.map grid_style ~f:(fun s -> At.style (Jstr.v s)) in
    let at = `R s :: at in
    Elwd.div ~at [ `R table_header; `R content; `R table_status ]

  let make dom_state ?scroll_target scroll_handler rows =
    let wrapper = make_wrapper dom_state ?scroll_target scroll_handler rows in
    make_table dom_state.layout wrapper
end

type ('data, 'error) state = {
  dom : Dom.state;
  (* The cache is some sort of LRU to keep live the content of recently seen
 rows *)
  mutable cache : ('data, 'error) row_data Lwd_table.row Cache.t;
  table : ('data, 'error) row_data Lwd_table.t;
  (* The [row_index] table is used to provide fast random access to the table's
     rows in the observer's callback *)
  row_index : (int, ('data, 'error) row_data Lwd_table.row) Hashtbl.t;
}

let new_cache () = Cache.create ~size:50

let prepare (state : ('data, 'error) state) ~total_items:total =
  let () = state.cache <- new_cache () in
  let i = ref 0 in
  let current_row = ref (Lwd_table.first state.table) in
  while Option.is_some !current_row || !i <= total - 1 do
    match !current_row with
    | Some row ->
        if !i <= total - 1 then
          let () = Hashtbl.replace state.row_index !i row in
          Lwd_table.set row { index = !i; content = None }
        else Lwd_table.unset row;
        incr i;
        current_row := Lwd_table.next row
    | None ->
        let set = { index = !i; content = None } in
        let row = Lwd_table.append ~set state.table in
        Hashtbl.add state.row_index !i row;
        incr i;
        current_row := Lwd_table.next row
  done

let make_spacer (state : Dom.state) n =
  let at = [ At.class' (Jstr.v "row_spacer") ] in
  let row_size = state.layout.row_height |> Utils.Unit.to_string in
  let height_n n = Printf.sprintf "height: calc(%s * %i);" row_size n in

  let style = At.style (Jstr.v @@ height_n n) in
  El.div ~at:(style :: at) []

module Spacer_monoid = struct
  let empty = (0, Lwd_seq.empty, 0)

  let concat dom (n, s, m) (p, s', q) =
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
            let spacer = Lwd.pure @@ make_spacer dom (m + p) in
            Lwd_seq.(concat s @@ concat (element spacer) s')
          else Lwd_seq.concat s s'
        in
        (n, s, q)

  let v dom = (empty, concat dom)
  let lwd_v dom = (Lwd.return empty, Lwd.map2 ~f:(concat dom))
end

(** Given the height of the wrapper element, the height of the table's rows and
    the scroll position, [compute_visible_rows] return the indices of currently
    visible rows. This includes indices of rows close but not visible rows
    ([bleeding]) so that they can be pre loaded to give the illusion to the user
    that they have always been there. The scroll direction is used to compute
    the scrolling speed and adjust bleeding consequently. *)
let compute_visible_rows (state : Dom.state) =
  let height elt =
    let jv = El.to_jv elt in
    Jv.get jv "offsetHeight" |> Jv.to_float
  in
  let div = FRef.get_exn state.wrapper_div in
  let scroll_y = El.scroll_y div in
  let direction = if scroll_y >. state.last_scroll_y then `Down else `Up in
  let () = state.last_scroll_y <- scroll_y in
  let visible_height = height div in
  let parent = Utils.Forward_ref.get_exn state.content_div in
  let row_height = Utils.Unit.to_px ~parent state.layout.row_height in
  let number_of_visible_rows =
    Int.of_float (ceil (visible_height /. row_height))
  in
  let bleeding = number_of_visible_rows in
  let scroll_y = scroll_y in
  let first_visible_row = int_of_float (scroll_y /. row_height) in
  let last_visible_row = first_visible_row + number_of_visible_rows in
  let first =
    let bleeding = match direction with `Up -> bleeding | _ -> bleeding / 2 in
    first_visible_row - bleeding |> max 0
  in
  let last =
    let bleeding =
      match direction with `Down -> bleeding | _ -> bleeding / 2
    in
    last_visible_row + bleeding
  in
  List.init (last - first) ~f:(fun i -> first + i)

let load_or_bump_in_cache (state : ('data, 'error) state) ~fetch
    ?(max_items = 200) rows =
  let load rows =
    (let data : ('data, _) Fut.result array = fetch (Array.map ~f:fst rows) in
     Array.iter2 rows data ~f:(fun (_, row) (data : ('data, _) Fut.result) ->
         (let open Option.Infix in
          let+ row_data = Lwd_table.get row in
          Lwd_table.set row { row_data with content = Some data })
         |> ignore))
    |> ignore
  in
  let unload row =
    Lwd_table.get row
    |> Option.iter (fun row_data ->
        Lwd_table.set row { row_data with content = None })
  in
  let cache, to_load =
    List.fold_left ~init:(state.cache, []) rows ~f:(fun (cache, acc) (i, row) ->
        ignore max_items (* cache is not configurable right now *);

        let cache, inserted = Cache.insert ~on_evict:unload cache i row in
        if inserted then (cache, (i, row) :: acc) else (cache, acc))
  in
  (* So much for the purely functionnal cache^^ *)
  state.cache <- cache;
  match Array.of_list to_load with [||] -> () | to_load -> load to_load

let update_visible_rows state fetch =
  let visible_rows_indexes = compute_visible_rows state.dom in
  (* todo: We do way too much work and rebuild the queue each
     time... it's very ineficient *)
  let visible_rows =
    List.filter_map
      ~f:(fun idx ->
        Hashtbl.get state.row_index idx |> Option.map (Pair.make idx))
      visible_rows_indexes
  in
  load_or_bump_in_cache state ~fetch
    ~max_items:(4 * List.length visible_rows)
    visible_rows

let index_of_row t row =
  Lwd_table.map_reduce
    (fun row _v -> (1, Some row, 0))
    ( (0, None, 0),
      fun (lb, lr, la) (rb, rr, ra) ->
        match (lr, rr) with
        | Some row', _ when Equal.physical row row' -> (lb, lr, la + rb + ra)
        | _, Some row' when Equal.physical row row' -> (lb + la + rb, rr, ra)
        | _ -> (lb + la + rb, rr, ra) )
    t
  |> Lwd.map ~f:(fun (i, _, _) -> (* todo check not found ? *) i)

type 'a loaded_state = Loaded of 'a | Unloaded

let make' (type data) ~(layout : Layout.fixed_row_height)
    (data_source : data Lwd_table.t)
    (renderer :
      int Lwd.var -> data Lwd_table.row -> data -> Elwd.t Lwd.t Lwd_seq.t Lwd.t)
    =
  let module RAList = CCRAL in
  let dom =
    Dom.
      {
        layout;
        content_div = Utils.Forward_ref.make ();
        wrapper_div = Utils.Forward_ref.make ();
        window_height = Lwd.var None;
        table_height = Lwd.var None;
        last_scroll_y = 0.;
      }
  in
  let cache = Lru.create ~on_remove:(fun _i f -> f ()) 20 in
  let row_size = layout.row_height |> Utils.Unit.to_string in
  let height = Printf.sprintf "height: %s !important;" row_size in
  let internal_seq =
    Lwd_table.map_reduce
      (fun row v -> Lwd_seq.element (Lwd.var 0, Lwd.var Unloaded, row, v))
      Lwd_seq.monoid data_source
  in
  let seq_index =
    Lwd_seq.fold_monoid
      (fun v -> RAList.(cons v empty))
      (RAList.empty, RAList.append)
      internal_seq
  in
  let scroll_handler =
    let on_scroll index =
      let visible_rows = compute_visible_rows dom in
      List.iter
        ~f:(fun i ->
          RAList.get index i
          |> Option.iter (fun (row_index, load_state, _row, _value) ->
              let () = Utils.set_if_different row_index i in
              Lru.use cache i (fun () -> Lwd.set load_state Unloaded);
              match Lwd.peek load_state with
              | Loaded () -> ()
              | _ -> Lwd.set load_state (Loaded ())))
        visible_rows
    in
    Lwd.map2 (Lwd.get dom.table_height) seq_index ~f:(fun _ index ->
        let () =
          (* We execute the handle once each time it changes to make sure the
             table is always up-to-date. *)
          (* Queuing prevents illegal updates during invalidation *)
          Window.queue_micro_task G.window (fun () -> on_scroll index)
        in
        let on_scroll =
          Utils.limit ~interval_ms:75 (fun () -> on_scroll index)
        in
        Elwd.handler Ev.scroll (fun _ev -> on_scroll ()))
  in
  let render (row_index, load_state, row, value) =
    let at = Attrs.add At.Name.class' (`P "lwdui-virtual-table-row") [] in
    let style = `P (At.style (Jstr.v height)) in
    Lwd.map (Lwd.get load_state) ~f:(function
      | Unloaded -> (1, Lwd_seq.empty, 0)
      | Loaded () ->
          let rendered_row = renderer row_index row value in
          ( 0,
            Lwd_seq.element
            @@ Elwd.div ~at:(style :: at) [ `S (Lwd_seq.lift rendered_row) ],
            0 ))
  in
  let rows =
    Lwd_seq.fold_monoid render (Spacer_monoid.lwd_v dom) internal_seq
  in
  let table_body =
    Lwd.map (Lwd.join rows) ~f:(fun (n, s, m) ->
        let result =
          if n > 0 then
            let first_spacer = Lwd.pure @@ make_spacer dom n in
            Lwd_seq.(concat (element first_spacer) s)
          else s
        in
        if m > 0 then
          let last_spacer = Lwd.pure @@ make_spacer dom m in
          Lwd_seq.(concat result (element last_spacer))
        else result)
  in
  let rows =
    let at = Attrs.O.(v (`P (C "lwdui-lazy-table-content"))) in
    let on_create = Utils.Forward_ref.set_exn dom.content_div in
    Elwd.div ~at ~on_create [ `S (Lwd_seq.lift table_body) ]
  in
  Dom.make dom scroll_handler rows

let make (type data error) ~(layout : Layout.fixed_row_height)
    ?(scroll_target : int Lwd.t option) (render : (data, error) row_renderer)
    (data_source : (data, error) data_source) =
  let state =
    {
      dom =
        {
          layout;
          content_div = Utils.Forward_ref.make ();
          wrapper_div = Utils.Forward_ref.make ();
          window_height = Lwd.var None;
          table_height = Lwd.var None;
          last_scroll_y = 0.;
        };
      cache = new_cache ();
      table = Lwd_table.make ();
      row_index = Hashtbl.create 2048;
    }
  in
  let row_size = layout.row_height |> Utils.Unit.to_string in
  let height = Printf.sprintf "height: %s !important;" row_size in
  let total_items, fetch =
    match data_source with Lazy { total_items; fetch } -> (total_items, fetch)
  in
  let table_body =
    let render _row { content; index } =
      let at = Attrs.add At.Name.class' (`P "lwdui-virtual-table-row") [] in
      let style = `P (At.style (Jstr.v height)) in
      match content with
      | Some data ->
          let rendered_row = render index data in
          ( 0,
            Lwd_seq.element
            @@ Elwd.div ~at:(style :: at) [ `S (Lwd_seq.lift rendered_row) ],
            0 )
      | None -> (1, Lwd_seq.empty, 0)
    in
    let rows =
      Lwd_table.map_reduce render (Spacer_monoid.v state.dom) state.table
    in
    Lwd.map rows ~f:(fun (n, s, m) ->
        let result =
          if n > 0 then
            let first_spacer = Lwd.pure @@ make_spacer state.dom n in
            Lwd_seq.(concat (element first_spacer) s)
          else s
        in
        if m > 0 then
          let last_spacer = Lwd.pure @@ make_spacer state.dom m in
          Lwd_seq.(concat result (element last_spacer))
        else result)
  in
  let rows =
    let at = Attrs.O.(v (`P (C "lwdui-lazy-table-content"))) in
    let on_create el = Utils.Forward_ref.set_exn state.dom.content_div el in
    Elwd.div ~at ~on_create [ `S (Lwd_seq.lift table_body) ]
  in
  let scroll_handler =
    Lwd.map fetch ~f:(fun fetch ->
        (* We use [last_update] to have regular debounced updates and the
           [timeout] to ensure that the last scroll event is always taken into
           account even it it happens during the debouncing interval. *)
        Console.log [ "POP ON SCROLL UPD" ];
        let update () = update_visible_rows state fetch in
        let update = Utils.limit update in
        Elwd.handler Ev.scroll (fun _ev -> update ()))
  in
  let wrapper =
    Dom.make_wrapper state.dom
      ~on_create:(fun () ->
        Utils.tap ~initial_trigger:true (Lwd.pair total_items fetch)
          ~f:(function total_items, fetch ->
            Console.log [ "Full refresh" ];
            prepare state ~total_items;
            update_visible_rows state fetch);
        Utils.tap ~initial_trigger:false
          (Lwd.pair fetch (Lwd.get state.dom.table_height))
          ~f:(function
            | fetch, _ ->
            Console.log [ "Height refresh" ];
            update_visible_rows state fetch))
      ?scroll_target scroll_handler rows
  in
  Dom.make_table layout wrapper

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

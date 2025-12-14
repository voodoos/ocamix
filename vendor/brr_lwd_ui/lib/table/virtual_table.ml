(** A virtual table that can handle large dataset.

    TODO: this is clearly over-engineered: the large lwd table that reduces to
    rows and placeholders with a monoid is elegant bu does not scale well. It
    might be possible to optimize it (especially the "uniqueue" LRU thingy), but
    having too large a lwd_table is probably a hard limit. *)

open Import
open Common
open Brrer
open Brr
open Brr_lwd
module FRef = Utils.Forward_ref
module Sort = Utils.Sort

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
  Data_source.Lazy { total_items = t.length; fetch }

(* The virtual table is a complex reactive component. Primarily, it reacts to
       changes of the [data_source] so that content in the table is properly
       refreshed when it does. Additionnaly it needs to react to multiple dom
       events, notably vertical resize of the container and scroll events, to ensure
       that the visible part of the talbe is always populated with rows. *)

let prepare (state : ('layout, 'data, 'error) state) ~total_items:total =
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
            let spacer = Lwd.pure @@ Dom.make_spacer dom (m + p) in
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
let compute_visible_rows (state : _ Dom.state) =
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
  let row_height = Css_length.to_px' parent state.layout#row_height in
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
  (first, last - first)

let load_or_bump_in_cache (state : ('layout, 'data, 'error) state) ~fetch rows =
  let load rows =
    (let data : ('data, _) Fut.result array = fetch (Array.map ~f:fst rows) in
     Array.iter2 rows data ~f:(fun (_, row) (data : ('data, _) Fut.result) ->
         (let open Option.Infix in
          let+ row_data = Lwd_table.get row in
          Lwd_table.set row { row_data with content = Some data })
         |> ignore))
    |> ignore
  in
  let to_load =
    List.fold_left ~init:[] rows ~f:(fun acc (i, row) ->
        let inserted = Lru.use' state.cache i row in
        if inserted then (i, row) :: acc else acc)
  in
  match Array.of_list to_load with [||] -> () | to_load -> load to_load

let update_visible_rows state fetch =
  let first, lenght = compute_visible_rows state.dom in
  let visible_rows = List.init lenght ~f:(fun i -> first + i) in
  let visible_rows =
    List.filter_map
      ~f:(fun idx ->
        Hashtbl.get state.row_index idx |> Option.map (Pair.make idx))
      visible_rows
  in
  load_or_bump_in_cache state ~fetch visible_rows

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

let make' ~(layout : 'data Layout.fixed_table) (data_source : 'data Lwd_table.t)
    renderer =
  let module RAList = CCRAL in
  let dom =
    Dom.
      {
        layout;
        content_div = Utils.Forward_ref.make ();
        wrapper_div = Utils.Forward_ref.make ();
        wrapper_width = Lwd.var None;
        wrapper_height = Lwd.var None;
        last_scroll_y = 0.;
      }
  in
  let cache =
    Lru.create ~on_remove:(fun _i load_state -> Lwd.set load_state Unloaded) 20
  in
  let row_size = layout#row_height |> Css_length.to_string in
  let height = Printf.sprintf "height: %s !important;" row_size in
  let row_count =
    Lwd_table.map_reduce (fun _row _v -> 1) (0, ( + )) data_source
  in
  let internal_seq =
    (* This counter is used to dedup rows when sorting *)
    let i = ref 0 in
    Lwd_table.map_reduce
      (fun row v ->
        incr i;
        Lwd_seq.element ((Lwd.var 0, Lwd.var Unloaded, row, v), !i))
      Lwd_seq.monoid data_source
  in
  let sorted_seq =
    Lwd.bind (Lwd.get dom.layout#sort_state) ~f:(function
      | None -> internal_seq
      | Some { compare = Compare sort; _ } ->
          let sort =
            Sort.Compare
              {
                proj = (fun ((_, _, _, v), i) -> (sort.proj v, i));
                compare =
                  (fun (v1, i1) (v2, i2) ->
                    let c = sort.compare v1 v2 in
                    if c = 0 then Int.compare i1 i2 else c);
              }
          in
          Lwd_seq.sort_uniq (Sort.compare sort) internal_seq)
  in
  let seq_index =
    Lwd_seq.fold_monoid
      (fun (v, _) -> RAList.(cons v empty))
      (RAList.empty, RAList.append)
      sorted_seq
  in
  let scroll_handler =
    let on_scroll index () =
      let first, lenght = compute_visible_rows dom in
      for i = first to first + lenght do
        let row_index, load_state, _row, _value =
          (* Allocs less, but is it safe ? *) RAList.get_exn index i
        in
        let () = Utils.set_if_different row_index i in
        (* TODO for both implementations: actually visible rows should be
                 refreshed last in the cache and not bleeding ones. *)
        Lru.use cache i load_state;
        match Lwd.peek load_state with
        | Loaded () -> ()
        | _ -> Lwd.set load_state (Loaded ())
      done
    in
    Lwd.map2 (Lwd.get dom.wrapper_height) seq_index ~f:(fun h index ->
        let () =
          Option.iter
            (fun h ->
              (* TODO: that's not a very thougtful heuristic. We could take the
                 bleeding into account.  *)
              2 * (Dom.number_of_fitting_rows_in dom h + 10)
              |> Lru.set_max_length cache)
            h
        in
        let () =
          (* We execute the handle once each time it changes to make sure the
             table is always up-to-date. *)
          (* Queuing prevents illegal updates during invalidation *)
          Window.queue_micro_task G.window (on_scroll index)
        in
        let on_scroll = Utils.limit ~interval_ms:25 (on_scroll index) in
        Elwd.handler Ev.scroll (fun _ev -> on_scroll ()))
  in
  let render ((row_index, load_state, row, value), _) =
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
  let rows = Lwd_seq.fold_monoid render (Spacer_monoid.lwd_v dom) sorted_seq in
  let rows = Dom.make_rows dom ~row_count (Lwd.join rows) in
  Dom.make dom scroll_handler rows

let make (type data error) ~(layout : data Layout.fixed_table)
    ?(scroll_target : int Lwd.t option) (render : (data, error) row_renderer)
    (data_source : (data, error) Data_source.t) =
  let state = new_state layout in
  let total_items, fetch =
    match data_source with Lazy { total_items; fetch } -> (total_items, fetch)
  in
  let rows =
    let style =
      let row_size = layout#row_height |> Css_length.to_string in
      let height = Printf.sprintf "height: %s !important;" row_size in
      `P (At.style (Jstr.v height))
    in
    let render _row { content; index } =
      let at = Attrs.add At.Name.class' (`P "lwdui-virtual-table-row") [] in
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
    Dom.make_rows state.dom ~row_count:total_items rows
  in
  let scroll_handler =
    let on_scroll fetch () = update_visible_rows state fetch in
    Lwd.map2 fetch (Lwd.get state.dom.wrapper_height) ~f:(fun fetch h ->
        (* We use [last_update] to have regular debounced updates and the
           [timeout] to ensure that the last scroll event is always taken into
           account even it it happens during the debouncing interval. *)
        let () =
          Option.iter
            (fun h ->
              (* TODO: that's not a very thougtful heuristic *)
              let new_cache_size =
                4 * (1 + Dom.number_of_fitting_rows_in state.dom h)
              in
              Console.log [ "New cache size: "; new_cache_size ];
              Lru.set_max_length state.cache new_cache_size)
            h
        in
        let () =
          (* We execute the handle once each time it changes to make sure the
             table is always up-to-date. *)
          (* Queuing prevents illegal updates during invalidation *)
          Window.queue_micro_task G.window (on_scroll fetch)
        in
        let on_scroll = Utils.limit ~interval_ms:25 (on_scroll fetch) in
        Elwd.handler Ev.scroll (fun _ev -> on_scroll ()))
  in
  let wrapper = Dom.make_wrapper state.dom ?scroll_target scroll_handler rows in
  let () =
    Utils.tap ~initial_trigger:true total_items ~f:(function total_items ->
        Console.log [ "Full refresh" ];
        prepare state ~total_items)
  in
  Dom.make_table state.dom wrapper

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

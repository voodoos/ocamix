open! Import
open Brr
open Brr_lwd_ui.Forms
open Db.Generic_schema

type status = Refreshing | Ready of int

let status = Lwd.var Refreshing
let grid_display = Lwd.var Button.Off

let filter_of_formula ~matcher formula =
  let open View.Selection in
  let string_of_chars chars = String.of_list (List.rev chars) in
  String.fold_left formula ~init:[] ~f:(fun acc char ->
      match (char, acc) with
      | '+', _ -> `One_of [] :: acc
      | '-', _ -> `None_of [] :: acc
      | c, `One_of l :: tl -> `One_of (c :: l) :: tl
      | c, `None_of l :: tl -> `None_of (c :: l) :: tl
      | _, _ -> acc)
  |> List.filter_map ~f:(function
    | `One_of chars ->
        let name = string_of_chars chars in
        if String.is_empty name then None else Some (One_of (matcher ~name))
    | `None_of chars ->
        let name = string_of_chars chars in
        if String.is_empty name then None else Some (None_of (matcher ~name)))

let libraries_choices =
  let open Field_checkboxes in
  let choices =
    Lwd_seq.fold_monoid
      (fun (_, l) ->
        Lwd_seq.map
          (fun ((key, l) : int * Db.Generic_schema.Collection.t) ->
            Check
              {
                value = key;
                id = l.name ^ "-ck-id";
                name = l.name ^ "-ck";
                label = (fun () -> [ `P (El.txt' l.name) ]);
                state = true;
              })
          l)
      (Lwd.return Lwd_seq.empty, Lwd.map2 ~f:Lwd_seq.concat)
      Servers.servers_libraries
  in
  make { name = "library-selection"; desc = Lwd.join choices }

(*
let genres_choices =
  let open Field_checkboxes in
  let at = Attrs.O.(`P (C "vertical-picker") @:: v (`P (C ""))) in
  let choices =
    Lwd_seq.map
      (fun (key, (count, { Genre.name; _ })) ->
        let text = Printf.sprintf "%s (%i)" name count in
        Check (key, [ `P (El.txt' text) ], true))
      (Lwd.map (Lwd.get view0_genres) ~f:Lwd_seq.of_list)
  in
  let { field; value } =
    make ~at { name = "genre-selection"; desc = choices }
  in
  let value =
    Lwd.map value ~f:(fun v ->
        let v = Int.Set.of_list @@ Lwd_seq.to_list v in
        Lwd.set selected_genres v;
        ignore @@ filter1_changed ();
        v)
  in
  Lwd.map2 field value ~f:(fun field _ -> field) *)

let view_kind =
  let open Field_select in
  let options =
    Lwd.pure (Lwd_seq.of_list [ ("albums", "Albums"); ("tracks", "Tracks") ])
  in
  make { name = "view-sort"; default = "date_added"; label = [] } options

let view0 =
  let open View in
  Lwd.map2 libraries_choices.value (Lwd.get view_kind.value)
    ~f:(fun libraries kind ->
      let src_views =
        Selection.One_of (Lwd_seq.to_list libraries |> List.map ~f:fst)
      in
      let kind = kind_of_string kind in
      let req = { kind; src_views; sort = Sort.Date_added; filters = [] } in
      let open Fut.Result_syntax in
      let* view = Worker_client.query Create_view req in
      let* genres =
        let+ genres = Worker_client.query Get_view_genres view in
        Int.Map.to_list genres
        |> List.sort ~cmp:(fun (_, (c1, _)) (_, (c2, _)) -> Int.compare c2 c1)
      in
      let+ artists =
        let+ artists = Worker_client.query Get_view_artists view in
        Int.Map.to_list artists
        |> List.sort ~cmp:(fun (_, { count = c1; _ }) (_, { count = c2; _ }) ->
            Int.compare c2 c1)
      in
      (view, genres, artists))

let genre_formula =
  let open Field_textinput in
  let placeholder = "+classi -opera" in
  make ~placeholder ~debounce:250
    { name = "genre-formula"; default = None; label = [] }

let artist_formula =
  let open Field_textinput in
  let placeholder = "+john -lennon" in
  make ~placeholder ~debounce:250
    { name = "artist-formula"; default = None; label = [] }

let f_search =
  let open Field_textinput in
  make ~debounce:250 { name = "pouet"; default = None; label = [] }

let f_sort =
  let open Field_select in
  let options =
    Lwd.pure
      (Lwd_seq.of_list [ ("date_added", "Date added"); ("name", "Name") ])
  in
  make { name = "view-sort"; default = "date_added"; label = [] } options

let f_order =
  let open Field_select in
  let options =
    Lwd.pure
      (Lwd_seq.of_list
         [ ("asc", "Asc"); ("desc", "Desc"); ("random", "Random") ])
  in
  make { name = "view-order"; default = "random"; label = [] } options

let view =
  let filters =
    Common.Utils.triple
      (Lwd.get genre_formula.value)
      (Lwd.get artist_formula.value)
      (Lwd.get f_search.value)
  in
  let sort = Lwd.get f_sort.value in
  Common.Utils.map3 view0 filters sort
    ~f:(fun view0 (genres_f, artists_f, name_f) sort ->
      let open View in
      let open Fut.Result_syntax in
      let* view, view0_genres, view0_artists = view0 in
      let sort = Sort.of_string sort in
      let genres =
        genres_f
        |> Option.map_or ~default:[] @@ fun genres_formula ->
           (* TODO that's not efficient *)
           (* Only (Lwd.peek selected_genres) *)
           let genres =
             List.map ~f:(fun (k, (_, g)) -> (k, g.Genre.canon)) view0_genres
           in
           let matcher ~name =
             let canon_name = canonicalize_string name in
             List.filter_map genres ~f:(fun (key, name) ->
                 if String.find ~sub:canon_name name >= 0 then Some key
                 else None)
             |> Int.Set.of_list
           in
           filter_of_formula ~matcher genres_formula
      in
      let artists =
        artists_f
        |> Option.map_or ~default:[] @@ fun artists_formula ->
           let artists =
             List.map
               ~f:(fun (count, { v; _ }) -> (count, v.Artist.canon))
               view0_artists
           in
           let matcher ~name =
             let canon_name = canonicalize_string name in
             List.filter_map artists ~f:(fun (key, name) ->
                 if String.find ~sub:canon_name name >= 0 then Some key
                 else None)
             |> Int.Set.of_list
           in
           filter_of_formula ~matcher artists_formula
      in
      let name = Option.get_or ~default:"" name_f in
      let filters = [ Search name; Genres genres; Artists artists ] in
      let req = { view.request with filters; sort } in
      let () = Lwd.set status Refreshing in
      let start_time = Performance.now_ms G.performance in
      let result =
        match req.kind with
        | Tracks ->
            let+ v = Worker_client.query Create_view req in
            (v, None)
        | Albums ->
            let+ v, a = Worker_client.query Create_album_view req in
            (v, Some a)
      in
      Fut.map
        (fun v ->
          let now = Performance.now_ms G.performance in
          let () = Lwd.set status (Ready (Float.to_int (now -. start_time))) in
          v)
        result)

let search_and_sort = [ `R f_sort.field; `R f_order.field; `R f_search.field ]

let library_chooser =
  let at = Attrs.O.(v (`P (C "vertical-picker"))) in
  Elwd.div ~at [ `R libraries_choices.field ]

let genre_chooser =
  let at = Attrs.O.(v (`P (C "genres-picker"))) in
  Elwd.div ~at
    [
      `P (El.txt' "Filter by genre: ");
      `R genre_formula.field;
      `R
        (Lwd.map (Lwd.get genre_formula.value) ~f:(function
          | None -> El.txt' ""
          | Some s -> El.txt' s));
    ]

let artist_chooser =
  let at = Attrs.O.(v (`P (C "artists-picker"))) in
  Elwd.div ~at [ `P (El.txt' " by artist: "); `R artist_formula.field ]

let display =
  let el, _, _ =
    Button.two_state ~state:grid_display (fun s ->
        [
          `R
            (Lwd.map s ~f:(function
              | On -> El.txt' "List"
              | Off -> El.txt' "Grid"));
        ])
  in
  el

let status =
  let spinner =
    Lwd.map (Lwd.get status) ~f:(function
      | Refreshing -> El.txt' "Refreshing"
      | Ready i ->
          let duration =
            if i > 1100 then
              let seconds = Float.of_int i /. 1000. in
              Printf.sprintf "%.*f s" 2 seconds
            else Printf.sprintf "%i ms" i
          in
          El.txt' @@ Printf.sprintf " in %s" duration)
    |> fun txt -> Elwd.span [ `R txt ]
  in
  let item_count =
    Lwd.bind view ~f:(fun fut_view ->
        let f =
          Fut.map
            (function
              | Error _ -> (0, 0.)
              | Ok ({ View.duration; item_count; _ }, _) ->
                  (item_count, duration))
            fut_view
        in
        let v = Common.Utils.var_of_fut ~init:(0, 0.) f in
        Lwd.map (Lwd.get v) ~f:(fun (count, duration) ->
            El.txt'
            @@ Printf.sprintf "%i results, %s" count
            @@ Duration.pp_approx_duration duration))
    |> fun txt -> Elwd.span [ `R txt ]
  in
  [ `R item_count; `R spinner ]

let bar =
  let at = Attrs.O.(v (`P (C "filters-row"))) in
  let first_row =
    Elwd.div ~at
      [
        `R library_chooser;
        `R view_kind.field;
        `R genre_chooser;
        `R artist_chooser;
      ]
  in
  let second_row = Elwd.div ~at (search_and_sort @ [ `R display ]) in
  let at = Attrs.O.(v (`P (C "filters-container"))) in
  Elwd.div ~at [ `R first_row; `R second_row ]

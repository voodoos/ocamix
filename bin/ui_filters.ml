open! Import
open Brr
open Brr_lwd_ui.Forms
open Db.Generic_schema

(* TODO this module has already seen multiple experimental reworks, it's due for
   a cleanup and refactor... *)

let selected_libraries = Lwd.var Lwd_seq.empty
let view0 = Lwd.var None
let view0_genres : (int * (int * Genre.t)) list Lwd.var = Lwd.var []
let view0_artists : (int * Artist.t info) list Lwd.var = Lwd.var []
let selected_genres = Lwd.var Int.Set.empty
let genre_formula = Lwd.var ""
let artist_formula = Lwd.var ""
let selected_sort = Lwd.var "date_added"
let selected_order = Lwd.var "desc"
let name_filter = Lwd.var ""

type status = Refreshing | Ready of int

let status = Lwd.var Refreshing

let view =
  Lwd.var
    View.
      {
        request =
          {
            kind = Audio;
            src_views = All;
            sort = Sort.Date_added;
            filters = [];
          };
        start_offset = 0;
        item_count = 0;
        duration = 0.;
      }

(* TODO: query language ?
    "rock jap"          = "rock" OR "jap"
    "rock + jap"        = "rock" AND "jap"
    "rock + jap punk"   = ("rock" AND "jap") OR "PUNK"
    "rock + (jap punk)" = "rock" AND ("jap" OR "PUNK")
    - rock              = NOT "rock"
    - rock - punk       = NOT "rock" AND NOT "punk" ????
    punk - rock         = "punk" AND NOT "rock" ????
    punk - rock jazz    = "punk" AND NOT "rock" OR JAZZ
*)
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

let filter1_changed () =
  let open View in
  let src_views =
    Selection.One_of (Lwd.peek selected_libraries |> Lwd_seq.to_list)
  in
  let sort = Sort.of_string @@ Lwd.peek selected_sort in
  let genres =
    (* TODO that's not efficient *)
    (* Only (Lwd.peek selected_genres) *)
    let genres =
      Lwd.peek view0_genres
      |> List.map ~f:(fun (k, (_, g)) -> (k, g.Genre.canon))
    in
    let matcher ~name =
      let canon_name = canonicalize_string name in
      List.filter_map genres ~f:(fun (key, name) ->
          if String.find ~sub:canon_name name >= 0 then Some key else None)
      |> Int.Set.of_list
    in
    filter_of_formula ~matcher (Lwd.peek genre_formula)
  in
  let artists =
    let artists =
      Lwd.peek view0_artists
      |> List.map ~f:(fun (count, { v; _ }) -> (count, v.Artist.canon))
    in
    let matcher ~name =
      let canon_name = canonicalize_string name in
      List.filter_map artists ~f:(fun (key, name) ->
          if String.find ~sub:canon_name name >= 0 then Some key else None)
      |> Int.Set.of_list
    in
    filter_of_formula ~matcher (Lwd.peek artist_formula)
  in
  let filters =
    [ Search (Lwd.peek name_filter); Genres genres; Artists artists ]
  in
  let req = { kind = Audio; src_views; sort; filters } in
  let open Fut.Result_syntax in
  let start_time = Performance.now_ms G.performance in
  Lwd.set status Refreshing;
  let+ view' = Worker_client.query (Create_view req) in
  let request_time =
    Float.to_int (Performance.now_ms G.performance -. start_time)
  in
  Lwd.set status (Ready request_time);
  Lwd.set view view'

let filter0_changed () =
  let open View in
  let src_views =
    Selection.One_of (Lwd.peek selected_libraries |> Lwd_seq.to_list)
  in
  let req = { kind = Audio; src_views; sort = Sort.Date_added; filters = [] } in
  let open Fut.Result_syntax in
  let+ view = Worker_client.query (Create_view req) in
  let+ genres = Worker_client.query (Get_view_genres view) in
  let+ artists = Worker_client.query (Get_view_artists view) in
  let sorted_genres =
    Int.Map.to_list genres
    |> List.sort ~cmp:(fun (_, (c1, _)) (_, (c2, _)) -> Int.compare c2 c1)
  in
  let sorted_artists =
    Int.Map.to_list artists
    |> List.sort ~cmp:(fun (_, { count = c1; _ }) (_, { count = c2; _ }) ->
           Int.compare c2 c1)
  in
  Lwd.set view0 (Some view);
  Lwd.set view0_genres sorted_genres;
  Lwd.set view0_artists sorted_artists;
  filter1_changed ()

let request_refresh =
  (* TODO a better model would be to move throttling to the inputs themselves
     and have a more nature form flow with lwd values.*)
  let throttler = Brr_utils.throttle ~delay_ms:250 ~delay:true in
  fun () -> throttler (fun () -> filter1_changed () |> ignore)

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
  let { field; value; _ } =
    make { name = "library-selection"; desc = Lwd.join choices }
  in
  let value =
    let value = Lwd_seq.map (fun (v, _) -> v) value in
    Lwd.map value ~f:(fun v ->
        Lwd.set selected_libraries v;
        ignore @@ filter0_changed ();
        v)
  in
  Lwd.map2 field value ~f:(fun field _ -> field)
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

let genre_formula =
  let open Field_textinput in
  let on_change ~init v =
    Lwd.set genre_formula v;
    if not init then request_refresh ()
  in
  let placeholder = "+classi -opera" in
  (make ~on_change ~placeholder
     { name = "genre-formula"; default = None; label = [] })
    .field

let artist_formula =
  let open Field_textinput in
  let on_change ~init v =
    Lwd.set artist_formula v;
    if not init then request_refresh ()
  in
  let placeholder = "+john -lennon" in
  make ~on_change ~placeholder
    { name = "artist-formula"; default = None; label = [] }

let search_and_sort =
  let f_search =
    let open Field_textinput in
    let on_change ~init v =
      Lwd.set name_filter v;
      if not init then request_refresh ()
    in
    make ~on_change { name = "pouet"; default = None; label = [] }
  in
  let f_sort =
    let open Field_select in
    let options =
      Lwd.pure
        (Lwd_seq.of_list [ ("date_added", "Date added"); ("name", "Name") ])
    in
    let on_change ~init v =
      Lwd.set selected_sort v;
      if not init then request_refresh ()
    in
    make ~on_change
      { name = "view-sort"; default = "date_added"; label = [] }
      options
  in
  let f_order =
    let open Field_select in
    let options =
      Lwd.pure
        (Lwd_seq.of_list
           [ ("asc", "Asc"); ("desc", "Desc"); ("random", "Random") ])
    in
    let on_change ~init v =
      Lwd.set selected_order v;
      if not init then request_refresh ()
    in
    make ~on_change
      { name = "view-order"; default = "random"; label = [] }
      options
  in
  [ `R f_sort.field; `R f_order.field; `R f_search.field ]

let library_chooser =
  let at = Attrs.O.(v (`P (C "vertical-picker"))) in
  Elwd.div ~at [ `R libraries_choices ]

let genre_chooser =
  let at = Attrs.O.(v (`P (C "genres-picker"))) in
  Elwd.div ~at [ `P (El.txt' "Filter by genre: "); `R genre_formula ]

let artist_chooser =
  let at = Attrs.O.(v (`P (C "artists-picker"))) in
  Elwd.div ~at [ `P (El.txt' " by artist: "); `R artist_formula.field ]

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
          El.txt' @@ Printf.sprintf "in %s" duration)
    |> fun txt -> Elwd.div [ `R txt ]
  in
  let item_count =
    Lwd.map (Lwd.get view) ~f:(fun { View.item_count; duration; _ } ->
        let duration = Duration.pp_approx_duration duration in
        El.txt' @@ Printf.sprintf "%i results, %s" item_count duration)
    |> fun txt -> Elwd.div [ `R txt ]
  in
  [ `R item_count; `R spinner ]

let bar =
  let at = Attrs.O.(v (`P (C "filters-row"))) in
  let first_row =
    Elwd.div ~at [ `R library_chooser; `R genre_chooser; `R artist_chooser ]
  in
  let second_row = Elwd.div ~at (search_and_sort @ status) in
  let at = Attrs.O.(v (`P (C "filters-container"))) in
  Elwd.div ~at [ `R first_row; `R second_row ]

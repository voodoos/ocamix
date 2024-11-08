open! Import
open Brr
open Brr_lwd_ui.Forms
open Db.Generic_schema

let selected_libraries = Lwd.var Lwd_seq.empty
let view0 = Lwd.var None
let view0_genres = Lwd.var Lwd_seq.empty
let selected_genres = Lwd.var Int.Set.empty
let selected_sort = Lwd.var "date_added"
let selected_order = Lwd.var "desc"
let name_filter = Lwd.var ""

let view =
  Lwd.var
    View.
      {
        request =
          {
            kind = Audio;
            src_views = Only [];
            sort = Sort.Date_added;
            filters = [];
          };
        start_offset = 0;
        item_count = 0;
      }

let filter0_changed () =
  let open View in
  let src_views = Only (Lwd.peek selected_libraries |> Lwd_seq.to_list) in
  let req = { kind = Audio; src_views; sort = Sort.Date_added; filters = [] } in
  let open Fut.Result_syntax in
  let* view = Worker_client.query (Create_view req) in
  let+ genres = Worker_client.query (Get_view_albums view) in
  let sorted_genres =
    Int.Map.to_list genres
    |> List.sort ~cmp:(fun (_, (c1, _)) (_, (c2, _)) -> Int.compare c2 c1)
  in
  Lwd.set view0 (Some view);
  Lwd.set view0_genres (Lwd_seq.of_list sorted_genres)

let filter1_changed () =
  let open View in
  let src_views = Only (Lwd.peek selected_libraries |> Lwd_seq.to_list) in
  let sort = Sort.of_string @@ Lwd.peek selected_sort in
  let genres =
    (* TODO that's not efficient *)
    Only (Lwd.peek selected_genres)
  in
  let filters = [ Search (Lwd.peek name_filter); Genres genres ] in
  let req = { kind = Audio; src_views; sort; filters } in
  let open Fut.Result_syntax in
  let+ view' = Worker_client.query (Create_view req) in
  Lwd.set view view'

let libraries_choices =
  let open Field_checkboxes in
  let choices =
    Lwd_seq.fold_monoid
      (fun (_, l) ->
        Lwd_seq.map
          (fun ((key, l) : int * Db.Generic_schema.Collection.t) ->
            Check (key, [ `P (El.txt' l.name) ], true))
          l)
      (Lwd.return Lwd_seq.empty, Lwd.map2 ~f:Lwd_seq.concat)
      Servers.servers_libraries
  in
  let { field; value } =
    make { name = "library-selection"; desc = Lwd.join choices }
  in
  let value =
    Lwd.map value ~f:(fun v ->
        Lwd.set selected_libraries v;
        ignore @@ filter0_changed ();
        ignore @@ filter1_changed ();
        v)
  in
  Lwd.map2 field value ~f:(fun field _ -> field)

let genres_choices =
  let open Field_checkboxes in
  let at = Attrs.O.(`P (C "vertical-picker") @:: v (`P (C ""))) in
  let choices =
    Lwd_seq.map
      (fun (key, (count, { Genre.name; _ })) ->
        let text = Printf.sprintf "%s (%i)" name count in
        Check (key, [ `P (El.txt' text) ], true))
      (Lwd.get view0_genres)
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
  Lwd.map2 field value ~f:(fun field _ -> field)

let search_and_sort =
  let f_search =
    let open Field_textinput in
    let on_change v =
      Lwd.set name_filter v;
      ignore @@ filter1_changed ()
    in
    make ~on_change { name = "pouet"; default = None; label = [] }
  in
  let f_sort =
    let open Field_select in
    let options =
      Lwd.pure
        (Lwd_seq.of_list [ ("date_added", "Date added"); ("name", "Name") ])
    in
    let on_change v =
      Lwd.set selected_sort v;
      ignore @@ filter1_changed ()
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
    let on_change v =
      Lwd.set selected_order v;
      ignore @@ filter1_changed ()
    in
    make ~on_change
      { name = "view-order"; default = "desc"; label = [] }
      options
  in
  [ `R f_sort.field; `R f_order.field; `R f_search.field ]

let library_chooser =
  let at = Attrs.O.(v (`P (C "vertical-picker"))) in
  Elwd.div ~at [ `R libraries_choices ]

let genre_chooser =
  let at = Attrs.O.(v (`P (C "genres-picker"))) in
  Elwd.div ~at [ `R genres_choices ]

let bar =
  let at = Attrs.O.(v (`P (C "filters-row"))) in
  let first_row = Elwd.div ~at [ `R library_chooser; `R genre_chooser ] in
  let second_row = Elwd.div ~at search_and_sort in
  let at = Attrs.O.(v (`P (C "filters-container"))) in
  Elwd.div ~at [ `R first_row; `R second_row ]

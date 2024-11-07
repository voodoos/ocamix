open! Import
open Brr
open Brr_lwd_ui.Forms
open Db.Generic_schema

let selected_libraries = Lwd.var Lwd_seq.empty
let view0 = Lwd.var None
let view0_genres = Lwd.var Lwd_seq.empty
let selected_genres = Lwd.var Lwd_seq.empty

let filter0_changed () =
  let open View in
  let src_views = Only (Lwd.peek selected_libraries |> Lwd_seq.to_list) in
  let req = { kind = Audio; src_views; sort = Sort.Date_added; filters = [] } in
  let open Fut.Result_syntax in
  let* view = Worker_client.query (Create_view req) in
  let+ genres = Worker_client.query (Get_view_albums view) in
  Lwd.set view0 (Some view);
  Lwd.set view0_genres (Lwd_seq.of_list genres)

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
        Console.log [ "value changed" ];
        Lwd.set selected_libraries v;
        ignore @@ filter0_changed ();
        v)
  in
  Lwd.map2 field value ~f:(fun field _ -> field)

let genres_choices =
  let open Field_checkboxes in
  let choices =
    Lwd_seq.map
      (fun { key; value = { Genre.name; _ } } ->
        Check (key, [ `P (El.txt' name) ], true))
      (Lwd.get view0_genres)
  in
  let { field; value } = make { name = "genre-selection"; desc = choices } in
  let value =
    Lwd.map value ~f:(fun v ->
        Console.log [ "value changed" ];
        Lwd.set selected_genres v;
        (* ignore @@ filter_changed (); *)
        v)
  in
  Lwd.map2 field value ~f:(fun field _ -> field)

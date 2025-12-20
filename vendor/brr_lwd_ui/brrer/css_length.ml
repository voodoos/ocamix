open Brr

(** Conversion between CSS units. Very early WIP. *)
type t = Px of float | Rem of float | Em of float | Fr of float

let j_font_size = Jstr.v "font-size"
let j_px = Jstr.v "px"
let j_em = Jstr.v "em"
let j_rem = Jstr.v "rem"
let j_fr = Jstr.v "fr"

let of_jstr s =
  (* TODO: proper parsing *)
  let open Jstr in
  let length = length s in
  let get_float suf_length = to_float (sub ~len:(length - suf_length) s) in
  if ends_with ~suffix:j_px s then Some (Px (get_float 2))
  else if ends_with ~suffix:j_em s then Some (Em (get_float 2))
  else if ends_with ~suffix:j_fr s then Some (Fr (get_float 2))
  else if ends_with ~suffix:j_rem s then Some (Rem (get_float 3))
  else None

let to_string = function
  | Px i -> Printf.sprintf "%fpx" i
  | Rem f -> Printf.sprintf "%frem" f
  | Em f -> Printf.sprintf "%fem" f
  | Fr f -> Printf.sprintf "%ffr" f

let to_jstr = function
  | Px i -> Jstr.(of_float i + j_px)
  | Rem f -> Jstr.(of_float f + j_rem)
  | Em f -> Jstr.(of_float f + j_em)
  | Fr f -> Jstr.(of_float f + j_fr)

let pp fmt t = Printf.fprintf fmt "%s" (to_string t)

let to_px' parent =
  let get_font_size_in_px parent =
    let font_size = El.computed_style j_font_size parent in
    match of_jstr font_size with
    | None -> 16.
    | Some (Px i) -> i
    | Some _ -> failwith "not implemented"
  in
  function
  | Px i -> i
  | Rem f ->
      let font_size = get_font_size_in_px (G.document |> Document.root) in
      f *. font_size
  | Em f ->
      let font_size = get_font_size_in_px parent in
      f *. font_size
  | Fr _ -> failwith "not implemented"

let to_px ?(parent = G.document |> Document.root) v = to_px' parent v

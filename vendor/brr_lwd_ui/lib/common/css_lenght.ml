open Import
open Brr

(** Conversion between CSS units. Very early WIP. *)
type t = Px of float | Rem of float | Em of float | Fr of float

let of_string s =
  (* TODO: proper parsing *)
  match String.chop_suffix ~suf:"px" s with
  | Some i -> (
      match Int.of_string i with
      | Some i -> Some (Px (float_of_int i))
      | None -> Float.of_string_opt i |> Option.map (fun i -> Px i))
  | None -> (
      match String.chop_suffix ~suf:"rem" s with
      | Some f -> Float.of_string_opt f |> Option.map (fun f -> Rem f)
      | None -> (
          match String.chop_suffix ~suf:"em" s with
          | Some f -> Float.of_string_opt f |> Option.map (fun f -> Rem f)
          | None -> None))

let to_string = function
  | Px i -> Printf.sprintf "%fpx" i
  | Rem f -> Printf.sprintf "%frem" f
  | Em f -> Printf.sprintf "%fem" f
  | Fr f -> Printf.sprintf "%ffr" f

let pp fmt t = Printf.fprintf fmt "%s" (to_string t)

let to_px ?(parent = G.document |> Document.root) =
  let get_font_size_in_px parent =
    let font_size =
      El.computed_style (Jstr.v "font-size") parent |> Jstr.to_string
    in
    match of_string font_size with
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

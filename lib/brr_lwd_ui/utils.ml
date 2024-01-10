open Std
open Brr

let is_pure_element = function
  | `P _ -> true
  | `R x -> Option.is_some (Lwd.is_pure x)
  | `S x -> Option.is_some (Lwd.is_pure x)

let pure t = `P t
let reactive t = `R t
let sequence t = `S t

module Unit = struct
  type t = Px of float | Rem of float | Em of float

  let of_string s =
    match String.chop_suffix ~suf:"px" s with
    | Some i -> Float.of_string_opt i |> Option.map (fun i -> Px i)
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

  let to_px ?(parent = G.document |> Document.root) =
    let get_font_size_in_px parent =
      let font_size =
        El.computed_style (Jstr.v "font-size") parent |> Jstr.to_string
      in
      match of_string font_size with Some (Px i) -> i | _ -> assert false
    in
    function
    | Px i -> i
    | Rem f ->
        let font_size = get_font_size_in_px (G.document |> Document.root) in
        f *. font_size
    | Em f ->
        let font_size = get_font_size_in_px parent in
        f *. font_size
end

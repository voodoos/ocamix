open Import
open Brr

type 'a one_maybe_reactive = [ `P of 'a | `R of 'a Lwd.t ]
type 'a maybe_reactive = [ 'a one_maybe_reactive | `S of 'a Lwd_seq.t Lwd.t ]

let is_pure_element = function
  | `P _ -> true
  | `R x -> Option.is_some (Lwd.is_pure x)
  | `S x -> Option.is_some (Lwd.is_pure x)

let pure t = `P t
let reactive t = `R t
let sequence t = `S t

module Unit = struct
  (** Conversion between CSS units. Very early WIP. *)
  type t = Px of float | Rem of float | Em of float

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
end

let listen ~f t =
  let root = Lwd.observe t in
  Lwd.set_on_invalidate root (fun _ -> f (Lwd.quick_sample root));
  Lwd.quick_sample root |> ignore

let map3 ~f a b c =
  Lwd.map2 a b ~f:(fun a b -> (a, b)) |> Lwd.map2 c ~f:(fun c (a, b) -> f a b c)

let triple a b c = map3 a b c ~f:(fun a b c -> (a, b, c))

module Forward_ref : sig
  type 'a t

  exception Not_set
  exception Already_set

  val make : unit -> 'a t
  val set_exn : 'a t -> 'a -> unit
  val get_exn : 'a t -> 'a
end = struct
  type 'a t = 'a option ref

  exception Not_set
  exception Already_set

  let make () = ref None

  let set_exn t v =
    match !t with None -> t := Some v | Some _ -> raise Already_set

  let get_exn t = match !t with None -> raise Not_set | Some v -> v
end

let var_of_fut ~init fut =
  let v = Lwd.var init in
  Fut.await fut (Lwd.set v);
  v

let wait_and_set v fut = Fut.await fut (Lwd.set v)

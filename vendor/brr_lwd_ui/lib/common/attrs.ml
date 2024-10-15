open Import

module Classes = struct
  include String.Set

  type nonrec t = t
  type modifier = Add of string list | Replace of string list

  let make l = of_list l
  let update t = function Add l -> add_list t l | Replace l -> of_list l

  let to_string t =
    let b = Buffer.create 32 in
    let first = ref true in
    iter
      (fun c ->
        if !first then (
          first := false;
          Printf.bprintf b "%s" c)
        else Printf.bprintf b " %s" c)
      t;
    Buffer.contents b

  let at_of_string c = Brr.At.class' @@ Jstr.v c

  module P = struct
    let to_at t =
      List.map (to_list t) ~f:(fun c -> `P (Brr.At.class' @@ Jstr.v c))
  end
end

open Brr
open Brr_lwd

type t = { classes : Classes.t; attrs : At.t Elwd.col }
(** Classes attributes are handled separately but are eventually translated to
    At.t *)

let empty = { classes = Classes.empty; attrs = [] }

let to_at ?id t =
  let at = List.rev_append t.attrs @@ Classes.P.to_at t.classes in
  match id with None -> at | Some id -> `P (At.id (Jstr.v id)) :: at

let classes l = { empty with classes = Classes.of_list l }

let union { classes; attrs } { classes = c; attrs = a } =
  { classes = Classes.union classes c; attrs = List.rev_append attrs a }

module Builder = struct
  type at = C of string | At of At.t

  let ( + ) t = function
    | C classname ->
        let classes = Classes.add classname t.classes in
        { t with classes }
    | At at ->
        let attrs = `P at :: t.attrs in
        { t with attrs }

  let with_id s t =
    let attrs = `P (At.id @@ Jstr.v s) :: t.attrs in
    to_at { t with attrs }
end

(* new API *)

let add at_name v at =
  let a =
    match v with
    | `P v -> `P (At.v at_name @@ Jstr.v v)
    | `R v -> `R (Lwd.map v ~f:(fun v -> At.v at_name @@ Jstr.v v))
    | `S _ -> failwith "TODO not implemented"
  in
  a :: at

let add_bool at_ v at = match v with false -> at | true -> `P at_ :: at

let add_str at_name v at =
  match v with "" -> at | v -> `P (At.v at_name @@ Jstr.v v) :: at

let add_opt at_name v at =
  match v with None -> at | Some v -> `P (At.v at_name @@ Jstr.v v) :: at

type at = C of string | A of At.t
type 'at t' = 'at Elwd.col

let map_col f = function
  | `P v -> `P (f v)
  | `R v -> `R (Lwd.map v ~f)
  | `S v -> `S (Lwd_seq.map f v)

let of_at = function A at -> at | C name -> At.class' (Jstr.v name)
let cons at t = map_col of_at at :: t
let class_ n = cons (map_col (fun n -> C n) n) []

module O = struct
  type nonrec at = at = C of string | A of At.t

  let v at = cons at []
  let ( @:: ) at t = cons at t
end

let with_id s t =
  let attrs = `P (At.id @@ Jstr.v s) :: t.attrs in
  to_at { t with attrs }

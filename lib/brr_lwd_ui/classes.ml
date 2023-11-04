open Std
module Set = String.Set

type t = Set.t
type modifier = Add of string list | Replace of string list

let make l = Set.of_list l
let update t = function Add l -> Set.add_list t l | Replace l -> Set.of_list l

let to_string t =
  let b = Buffer.create 32 in
  let first = ref true in
  Set.iter
    (fun c ->
      if !first then (
        first := false;
        Printf.bprintf b "%s" c)
      else Printf.bprintf b " %s" c)
    t;
  Buffer.contents b

let to_list = Set.to_list
let union = Set.union
let at_of_string c = Brr.At.class' @@ Jstr.v c

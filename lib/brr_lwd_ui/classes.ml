open Std
module Set = String.Set

type t = Set.t
type modifier = Add of string list | Replace of string list

let make l = Set.of_list l
let update t = function Add l -> Set.add_list t l | Replace l -> Set.of_list l

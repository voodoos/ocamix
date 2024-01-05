open Std

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
(** Classes attributes are handled separately but are eventually translated to At.t *)

let empty = { classes = Classes.empty; attrs = [] }

let to_at ?id t =
  let at = List.rev_append t.attrs @@ Classes.P.to_at t.classes in
  match id with None -> at | Some id -> `P (At.id (Jstr.v id)) :: at

let classes l = { empty with classes = Classes.of_list l }

let union { classes; attrs } { classes = c; attrs = a } =
  { classes = Classes.union classes c; attrs = List.rev_append attrs a }

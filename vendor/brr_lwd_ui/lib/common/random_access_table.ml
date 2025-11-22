module RAList = CCRAL

(* If CCRAL follows the spec:
    - [append] must take time bounded by O(log(n + m))
    - [get] must take time bounded by O(log(n)) *)

(* Alternatively, we could use a Rope here. There is an implementation of rope
   of any type in ocaml-bazaar:
   https://github.com/backtracking/ocaml-bazaar/blob/main/rope.mli *)

type 'a t = {
  table : 'a Lwd_table.t;
  index : 'a Lwd_table.row RAList.t Lwd.t;
  length : int Lwd.t;
}

let make ?(from_table = Lwd_table.make ()) () =
  let index =
    Lwd_table.map_reduce
      (fun row _v -> RAList.(cons row empty))
      (RAList.empty, RAList.append)
      from_table
  in
  let length = Lwd_table.map_reduce (fun _row _v -> 1) (0, ( + )) from_table in
  { table = from_table; index; length }

let get t i = Lwd.map t.index ~f:(Fun.flip RAList.get i)

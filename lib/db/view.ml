open Std

module Sort = struct
  type criteria = Date_added
  type direction = Asc | Desc
  type t = Some of criteria * direction | Random
end

module Order = struct
  type t = Initial | Custom of int array

  let of_sort ~size = function
    | Sort.Random ->
        let tbl = Array.init size ~f:Fun.id in
        let () = Array.shuffle tbl in
        Custom tbl
    | _ -> Initial

  let apply t i =
    match t with Initial -> i | Custom a -> (* todo check bounds *) a.(i)
end

type req = { sort : Sort.t; filters : unit list }

type t = {
  uuid : Uuidm.t;
  request : req;
  order : Order.t;
  start_offset : int;
  item_count : int;
}

let req ?(sort = Sort.(Some (Date_added, Desc))) ?(filters = []) () =
  { sort; filters }

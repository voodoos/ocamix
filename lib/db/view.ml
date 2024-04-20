open Std

module Sort = struct
  type criteria = Date_added
  type direction = Asc | Desc
  type t = Some of criteria * direction | Random
end

(** Some sorts require a custom ordering which is done using a table of indexes. For example, to get a random sort we simple shuffle an array which size is the one of the result. *)
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

type 'a selection = All | Only of 'a list
type kind = Audio
type filter = Search of string

type req = {
  kind : kind;
  src_views : string selection;
  sort : Sort.t;
  filters : filter list;
}

type t = {
  uuid : Uuidm.t;
  request : req;
  order : Order.t;
  start_offset : int;
  item_count : int;
}

let item_count t = t.item_count - t.start_offset

let req kind ?(src_views = All) ?(sort = Sort.(Some (Date_added, Desc)))
    ?(filters = []) () =
  { kind; src_views; sort; filters }

let hash req = Hashtbl.hash (req.src_views, req.filters)

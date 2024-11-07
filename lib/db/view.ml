open Std

(** Some sorts require a custom ordering which is done using a table of indexes.
    For example, to get a random sort we simple shuffle an array which size is
    the one of the result. *)
module Order = struct
  type t = Initial | Asc | Desc | Custom of int array

  let random ~size =
    let tbl = Array.init size ~f:Fun.id in
    let () = Array.shuffle tbl in
    Custom tbl

  let of_string ~size = function
    | "random" -> random ~size
    | "asc" -> Asc
    | "desc" -> Desc
    | _ -> Initial

  let apply t ~size i =
    match t with
    | Initial | Asc -> i
    | Desc -> size - 1 - i
    | Custom a -> (* todo check bounds *) a.(i)
end

module Sort = struct
  type t = Date_added | Name

  let of_string = function
    | "date_added" -> Date_added
    | "name" -> Name
    | _ -> Date_added
end

type 'a selection = All | Only of 'a list
type kind = Audio
type filter = Search of string

type req = {
  kind : kind;
  src_views : int selection;
  sort : Sort.t;
  filters : filter list;
}

type t = { request : req; start_offset : int; item_count : int }
type ranged = { view : t; first : int; last : int; order : Order.t }

let item_count t = t.item_count - t.start_offset

let req kind ?(src_views = All) ?(sort = Sort.Date_added) ?(filters = []) () =
  { kind; src_views; sort; filters }

let hash req = Hashtbl.hash (req.src_views, req.filters)

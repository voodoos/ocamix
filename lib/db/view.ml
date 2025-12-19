open Std

(** Some sorts require a custom ordering which is done using a table of indexes.
    For example, to get a random sort we simple shuffle an array which size is
    the one of the result. *)
module Order = struct
  type t = Initial | Asc | Desc | Custom of int array [@@deriving jsont]

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
  type t = Date_added | Name [@@deriving jsont]

  let of_string = function
    | "date_added" -> Date_added
    | "name" -> Name
    | _ -> Date_added
end

module Selection = struct
  type 'a t = All | One_of of 'a | None_of of 'a [@@deriving jsont]

  let map ~f = function
    | All -> All
    | One_of s -> One_of (f s)
    | None_of s -> None_of (f s)
end

open Selection

type kind = Audio [@@deriving jsont]

let int_set_jsont = Encodings.set_jsont Jsont.int

type filter =
  | Search of string
  | Genres of (Int.Set.t[@jsont int_set_jsont]) Selection.t list
  | Artists of (Int.Set.t[@jsont int_set_jsont]) Selection.t list
[@@deriving jsont]

type req = {
  kind : kind;
  src_views : int list Selection.t;
  sort : Sort.t;
  filters : filter list;
}
[@@deriving jsont]

let hash { kind; src_views; sort; filters } =
  let filter l =
    List.map l ~f:(function
      | All -> "all"
      | One_of s ->
          String.concat ~sep:";"
          @@ ("oneof" :: (Int.Set.to_list s |> List.map ~f:string_of_int))
      | None_of s ->
          String.concat ~sep:";"
          @@ ("noneof" :: (Int.Set.to_list s |> List.map ~f:string_of_int)))
    |> String.concat ~sep:";"
  in
  let filters =
    List.map filters ~f:(fun f ->
        match f with
        | Search s -> s
        | Genres l -> "genres" ^ filter l
        | Artists l -> "artists" ^ filter l)
  in
  Hash.poly (kind, src_views, sort, filters)

type t = {
  request : req;
  start_offset : int;
  item_count : int;
  duration : Duration.t;
}
[@@deriving jsont]

type ranged = { view : t; first : int; last : int; order : Order.t }

let item_count t = t.item_count - t.start_offset

let req kind ?(src_views = All) ?(sort = Sort.Date_added) ?(filters = []) () =
  { kind; src_views; sort; filters }

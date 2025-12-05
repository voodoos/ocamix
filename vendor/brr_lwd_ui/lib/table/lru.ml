type ('a, 'b) cell = {
  key : 'a;
  value : 'b;
  mutable prev : ('a, 'b) cell option;
  mutable next : ('a, 'b) cell option;
}

type ('a, 'b) t = {
  table : ('a, ('a, 'b) cell) Hashtbl.t;
  mutable first : ('a, 'b) cell option;
  mutable last : ('a, 'b) cell option;
  on_remove : 'a -> 'b -> unit;
  mutable max_length : int;
}

let create ?(on_remove = fun _ _ -> ()) max_length =
  assert (max_length > 1);
  {
    table = Hashtbl.create max_length;
    first = None;
    last = None;
    on_remove;
    max_length;
  }

let set_max_length t max_Length = t.max_length <- max_Length

let remove_last t =
  match t.last with
  | None -> ()
  | Some cell ->
      let () =
        match cell.prev with
        | None -> t.first <- None
        | Some p -> p.next <- None
      in
      t.last <- cell.prev;
      Hashtbl.remove t.table cell.key;
      t.on_remove cell.key cell.value

(* [use' t key value] inserts [value] in the cache [t] with [key]. If the cache
  already contains a value with that key: the record is marked as used recently,
  its value stays unchanged and the function returns [false]. If a new record
  was created [true] is returned. *)
let use' t key value =
  for _ = 0 to Hashtbl.length t.table - t.max_length do
    remove_last t
  done;
  match Hashtbl.find_opt t.table key with
  | None ->
      let cell = { key; value; prev = None; next = t.first } in
      Hashtbl.add t.table key cell;
      begin match (t.first, t.last) with
      | None, None ->
          t.first <- Some cell;
          t.last <- Some cell
      | Some f, _ ->
          f.prev <- Some cell;
          t.first <- Some cell
      | None, Some _ -> assert false
      end;
      true
  | Some cell ->
      (* 3 cases *)
      begin match (cell.prev, cell.next) with
      | None, None ->
          (* Already the only cell, doing nothing *)
          ()
      | None, Some _n ->
          (* Already the first cell, doing nothing *)
          ()
      | Some p, None ->
          (* Remove the cell from the chain *)
          cell.prev <- None;
          cell.next <- t.first;
          p.next <- None;

          (* Put it in front *)
          t.first |> Option.iter (fun f -> f.prev <- Some cell);
          t.first <- Some cell;

          (* Was the last cell, must update the pointer *)
          t.last <- Some p
      | Some p, Some n ->
          (* Remove the cell from the chain *)
          cell.prev <- None;
          cell.next <- t.first;
          p.next <- Some n;
          n.prev <- Some p;

          (* Put it in front *)
          t.first |> Option.iter (fun f -> f.prev <- Some cell);
          t.first <- Some cell
      end;
      false

let use t key value = ignore @@ use' t key value

let debug_get_list t =
  let rec aux acc = function
    | None -> List.rev acc
    | Some { key; next; _ } -> aux (key :: acc) next
  in
  aux [] t.first

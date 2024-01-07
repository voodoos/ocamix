module Sort = struct
  type criteria = Date_added | Random
  type direction = Asc | Desc
  type t = criteria * direction
end

type req = { sort : Sort.t; filters : unit list }
type t = { uuid : Uuidm.t; request : req; start_index : int; item_count : int }

let req ?(sort = Sort.(Date_added, Desc)) ?(filters = []) () = { sort; filters }

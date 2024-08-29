open Brr

let global = Jv.get Jv.global "yjs"

type observer = Jv.t

module Event = struct
  type t = Jv.t

  external of_jv : Jv.t -> 'a = "%identity"

  type changes = { delta : Jv.t }

  let changes t =
    let changes_jv = Jv.get t "changes" in
    { delta = Jv.get changes_jv "delta" }
end

module Array = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let insert (t : t) i (v : Jv.t array) =
    let content = Jv.of_array Fun.id v in
    ignore (Jv.call t "insert" [| Jv.of_int i; content |])

  let observe t f : observer =
    let t = to_jv t in
    let callback e = f (Event.of_jv e) in
    let cb = Jv.repr callback in
    ignore (Jv.call t "observe" [| cb |]);
    cb
end

module Map = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let get (t : t) ~key = Jv.call t "get" [| Jv.of_string key |]

  let set (t : t) ~key value =
    ignore (Jv.call t "set" [| Jv.of_string key; value |])

  let entries t = Jv.call t "entries" [||] |> Jv.It.iterator

  let observe t f : observer =
    let t = to_jv t in
    let callback e = f (Event.of_jv e) in
    let cb = Jv.repr callback in
    ignore (Jv.call t "observe" [| cb |]);
    cb
end

module Doc = struct
  let global = Jv.get global "Doc"

  type t = Jv.t

  external of_jv : Jv.t -> 'a = "%identity"

  let get_array t name : Array.t =
    Jv.call t "getArray" [| Jv.of_string name |] |> Array.of_jv

  let get_map t name : Map.t =
    Jv.call t "getMap" [| Jv.of_string name |] |> Array.of_jv

  let make () = Jv.new' global [||]
end

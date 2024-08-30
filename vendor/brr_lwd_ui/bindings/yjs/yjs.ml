open Brr

let yjs = Jv.get Jv.global "yjs"
let array_class = Jv.get yjs "Array"
let map_class = Jv.get yjs "Map"

type observer = Jv.t

module Event = struct
  type 'change t = (Jv.t -> 'change) * Jv.t
  type 'change changes = { delta : 'change array }

  external of_jv : Jv.t -> 'a t = "%identity"

  let of_jv ~change_of_jv jv = (change_of_jv, jv)

  let changes (change_of_jv, jv) =
    let changes_jv = Jv.get jv "changes" in
    { delta = Jv.to_array change_of_jv @@ Jv.get changes_jv "delta" }
end

module rec Array : sig
  type t
  type value = [ `Jv of Jv.t | `Map of Map.t | `Array of Array.t ]
  type change = Retain of int | Insert of value array | Delete of int

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"
  val make : unit -> t
  val insert : t -> int -> value array -> unit
  val iter : t -> f:(index:int -> value -> t -> unit) -> unit
  val observe : t -> (change Event.t -> unit) -> observer
end = struct
  type t = Jv.t
  type value = [ `Jv of Jv.t | `Map of Map.t | `Array of Array.t ]
  type change = Retain of int | Insert of value array | Delete of int

  let value_of_jv jv =
    if Jv.instanceof jv ~cons:array_class then `Array (Array.of_jv jv)
    else if Jv.instanceof jv ~cons:map_class then `Map (Map.of_jv jv)
    else `Jv jv

  let change_of_jv jv =
    match Jv.find jv "retain" with
    | Some i -> Retain (Jv.to_int i)
    | None -> (
        match Jv.find jv "insert" with
        | Some a -> Insert (Jv.to_array value_of_jv a)
        | None -> (
            match Jv.find jv "delete" with
            | Some i -> Delete (Jv.to_int i)
            | None -> assert false))

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let make () = Jv.new' array_class [||]

  let insert t i v =
    let content =
      Jv.of_array
        (function `Jv jv -> jv | `Map m -> Map.to_jv m | `Array a -> a)
        v
    in
    ignore (Jv.call t "insert" [| Jv.of_int i; content |])

  let iter t ~f =
    let f =
      Jv.repr (fun value index arr ->
          let index = Jv.to_int index in
          f ~index (value_of_jv value) arr)
    in
    ignore (Jv.call t "forEach" [| f |])

  let observe t f : observer =
    let t = to_jv t in
    let callback e = f (Event.of_jv ~change_of_jv e) in
    let cb = Jv.repr callback in
    ignore (Jv.call t "observe" [| cb |]);
    cb
end

and Map : sig
  type t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"
  val make : unit -> t

  type value = [ `Jv of Jv.t | `Map of Map.t | `Array of Array.t ]
  type change = Retain of int | Insert of value | Delete of int

  val get : t -> key:string -> value
  val set : t -> key:string -> value -> unit
  val entries : t -> Jv.It.t
  val observe : t -> (change Event.t -> unit) -> observer
end = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  type value = [ `Jv of Jv.t | `Map of Map.t | `Array of Array.t ]
  type change = Retain of int | Insert of value | Delete of int

  let value_of_jv jv =
    if Jv.instanceof jv ~cons:array_class then `Array (Array.of_jv jv)
    else if Jv.instanceof jv ~cons:map_class then `Map (Map.of_jv jv)
    else `Jv jv

  let change_of_jv jv =
    match Jv.find jv "retain" with
    | Some i -> Retain (Jv.to_int i)
    | None -> (
        match Jv.find jv "insert" with
        | Some a -> Insert (value_of_jv a)
        | None -> (
            match Jv.find jv "delete" with
            | Some i -> Delete (Jv.to_int i)
            | None -> assert false))

  let make () = Jv.new' map_class [||]
  let get (t : t) ~key = Jv.call t "get" [| Jv.of_string key |] |> value_of_jv

  let set (t : t) ~key value =
    let value =
      match value with `Jv jv -> jv | `Map m -> m | `Array a -> Array.to_jv a
    in
    ignore (Jv.call t "set" [| Jv.of_string key; value |])

  let entries t = Jv.call t "entries" [||] |> Jv.It.iterator

  let observe t f : observer =
    let t = to_jv t in
    let callback e = f (Event.of_jv ~change_of_jv e) in
    let cb = Jv.repr callback in
    ignore (Jv.call t "observe" [| cb |]);
    cb
end

module Doc = struct
  let global = Jv.get yjs "Doc"

  type t = Jv.t

  external of_jv : Jv.t -> 'a = "%identity"

  let get_array t name : Array.t =
    Jv.call t "getArray" [| Jv.of_string name |] |> Array.of_jv

  let get_map t name : Map.t =
    Jv.call t "getMap" [| Jv.of_string name |] |> Map.of_jv

  let make () = Jv.new' global [||]
end

module StringMap = Map.Make (String)

let text_class = Jv.get Global.yjs "Text"
let array_class = Jv.get Global.yjs "Array"
let map_class = Jv.get Global.yjs "Map"

type observer = Jv.t

module Event = struct
  type 'change t = (Jv.t -> 'change) * Jv.t
  type 'change changes = { delta : 'change array }

  let of_jv ~change_of_jv jv = (change_of_jv, jv)

  let changes (change_of_jv, jv) =
    let changes_jv = Jv.get jv "changes" in
    { delta = Jv.to_array change_of_jv @@ Jv.get changes_jv "delta" }
end

module Text = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let make ?initial_content () =
    let args =
      match initial_content with None -> [||] | Some s -> [| Jv.of_string s |]
    in
    Jv.new' text_class args
end

module rec Array : sig
  type t

  type value =
    [ `Jv of Jv.t | `Text of Text.t | `Map of Map.t | `Array of Array.t ]

  type change = Retain of int | Insert of value array | Delete of int

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"
  val make : unit -> t
  val insert : t -> int -> value array -> unit
  val delete : t -> int -> int -> unit
  val push : t -> value array -> unit
  val iter : t -> f:(index:int -> value -> t -> unit) -> unit
  val observe : t -> (change Event.t -> unit) -> observer
end = struct
  type t = Jv.t

  type value =
    [ `Jv of Jv.t | `Text of Text.t | `Map of Map.t | `Array of Array.t ]

  type change = Retain of int | Insert of value array | Delete of int

  let value_of_jv jv =
    if Jv.instanceof jv ~cons:array_class then `Array (Array.of_jv jv)
    else if Jv.instanceof jv ~cons:map_class then `Map (Map.of_jv jv)
    else if Jv.instanceof jv ~cons:text_class then `Text (Text.of_jv jv)
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
        (function
          | `Jv jv -> jv
          | `Text m -> Text.to_jv m
          | `Map m -> Map.to_jv m
          | `Array a -> a)
        v
    in
    ignore (Jv.call t "insert" [| Jv.of_int i; content |])

  let delete t index lenght =
    ignore (Jv.call t "delete" [| Jv.of_int index; Jv.of_int lenght |])

  let push t v =
    let content =
      Jv.of_array
        (function
          | `Jv jv -> jv
          | `Text m -> Text.to_jv m
          | `Map m -> Map.to_jv m
          | `Array a -> a)
        v
    in
    ignore (Jv.call t "push" [| content |])

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

  type value =
    [ `Jv of Jv.t | `Text of Text.t | `Map of Map.t | `Array of Array.t ]

  module Event : sig
    type t

    external of_jv : Jv.t -> t = "%identity"

    type action = Add | Update | Delete

    type change = {
      action : action;
      new_value : value option;
      old_value : value option;
    }

    val keys_changes : t -> change StringMap.t
  end

  val get : t -> key:string -> value option
  val set : t -> key:string -> value -> unit
  val delete : t -> string -> unit
  val entries : t -> Jv.It.t

  val fold_entries :
    t -> f:(string -> value -> 'acc -> 'acc) -> init:'acc -> 'acc

  val observe : t -> (Event.t -> unit) -> observer
end = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  type value =
    [ `Jv of Jv.t | `Text of Text.t | `Map of Map.t | `Array of Array.t ]

  let value_of_jv jv =
    if Jv.instanceof jv ~cons:array_class then `Array (Array.of_jv jv)
    else if Jv.instanceof jv ~cons:map_class then `Map (Map.of_jv jv)
    else if Jv.instanceof jv ~cons:text_class then `Text (Text.of_jv jv)
    else `Jv jv

  let make () = Jv.new' map_class [||]

  let get (t : t) ~key =
    Jv.call t "get" [| Jv.of_string key |] |> Jv.to_option value_of_jv

  module Event = struct
    type t = Jv.t

    let map_of_jv = of_jv

    external of_jv : Jv.t -> t = "%identity"

    type action = Add | Update | Delete

    type change = {
      action : action;
      new_value : value option;
      old_value : value option;
    }

    let target t = Jv.get t "target" |> map_of_jv

    let change_of_jv obj =
      let action =
        match Jv.get obj "action" |> Jv.to_string with
        | "add" -> Add
        | "update" -> Update
        | "delete" -> Delete
        | s ->
            raise
            @@ Invalid_argument (Printf.sprintf "%S is not a valid action" s)
      in
      let old_value =
        let jv = Jv.get obj "oldValue" in
        if Jv.is_none jv then None else Some (value_of_jv jv)
      in
      { action; new_value = None; old_value }

    let keys_changes t =
      let key_map = Jv.get (Jv.get t "changes") "keys" in
      let entries = Jv.call key_map "entries" [||] in
      Jv.It.fold_bindings ~key:Jv.to_string ~value:change_of_jv
        (fun k v acc ->
          let new_value = get (target t) ~key:k in
          StringMap.add k { v with new_value } acc)
        entries StringMap.empty
  end

  let set (t : t) ~key value =
    let value =
      match value with
      | `Jv jv -> jv
      | `Text t -> Text.to_jv t
      | `Map m -> m
      | `Array a -> Array.to_jv a
    in
    ignore (Jv.call t "set" [| Jv.of_string key; value |])

  let delete (t : t) key = ignore @@ Jv.call t "delete" [| Jv.of_string key |]
  let entries t = Jv.call t "entries" [||] |> Jv.It.iterator

  let fold_entries t ~f ~init =
    Jv.It.fold_bindings ~key:Jv.to_string ~value:value_of_jv
      (fun k v acc -> f k v acc)
      (entries t) init

  let observe t f : observer =
    let t = to_jv t in
    let callback e = f (Event.of_jv e) in
    let cb = Jv.repr callback in
    ignore (Jv.call t "observe" [| cb |]);
    cb
end

module Doc = struct
  let global = Jv.get Global.yjs "Doc"

  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"
  external to_jv : t -> Jv.t = "%identity"

  let get_text t name : Text.t =
    Jv.call t "getText" [| Jv.of_string name |] |> Text.of_jv

  let get_array t name : Array.t =
    Jv.call t "getArray" [| Jv.of_string name |] |> Array.of_jv

  let get_map t name : Map.t =
    Jv.call t "getMap" [| Jv.of_string name |] |> Map.of_jv

  let make () = Jv.new' global [||]

  let transact t f =
    let callback = Jv.callback ~arity:1 f in
    Jv.call t "transact" [| callback |] |> ignore
end

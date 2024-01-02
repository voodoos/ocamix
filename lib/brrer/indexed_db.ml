open Brr

module Events = struct
  module Version_change = struct
    type t = Jv.t

    let old_version t = Jv.to_int @@ Jv.get t "oldVersion"
    let new_version t = Jv.to_int @@ Jv.get t "newVersion"
  end

  let upgrade_needed : Version_change.t Ev.type' =
    Ev.Type.create (Jstr.v "upgradeneeded")

  let success : Ev.Type.void Ev.type' = Ev.Type.void (Jstr.v "success")
end

module Request = struct
  type 'a t = { jv : Jv.t; of_jv : Jv.t -> 'a }

  external of_jv : Jv.t -> 'a = "%identity"

  let of_jv ~f j = { jv = of_jv j; of_jv = f }

  let result (type a) (t : a t) : a =
    (* todo this is wrong *)
    Jv.get t.jv "result" |> t.of_jv

  let on_success (type a) ~(f : Ev.Type.void Ev.t -> a t -> unit) (t : a t) =
    let f ev =
      let req : a t =
        Ev.current_target ev |> Ev.target_to_jv |> of_jv ~f:t.of_jv
      in
      f ev req
    in
    ignore @@ Ev.listen Events.success f (Ev.target_of_jv t.jv);
    t
end

module type Store_content_intf = sig
  type t
  type key

  val name : string
  val to_jv : t -> Jv.t
  val of_jv : Jv.t -> (t, [ `Msg of string ]) Result.t
  val key_to_jv : key -> Jv.t
  val key_of_jv : Jv.t -> key
  val key_path : string (* todo key_path should be optional *)
  val get_key : t -> key
end

module Direction = struct
  type t = Next | Next_unique | Prev | Prev_unique

  let to_string = function
    | Next -> "next"
    | Next_unique -> "nextUnique"
    | Prev -> "prev"
    | Prev_unique -> "prevUnique"

  let of_string = function
    | "next" -> Next
    | "nextUnique" -> Next_unique
    | "prev" -> Prev
    | "prevUnique" -> Prev_unique
    | s -> raise (Invalid_argument s)

  let to_jv d = Jv.of_string (to_string d)
  let of_jv j = of_string (Jv.to_string j)
end

module type Object_store_intf = sig
  type t

  val of_jv : Jv.t -> t

  module Content : Store_content_intf

  module Cursor : sig
    type t

    val key : t -> Content.key option
    val advance : int -> t -> t
    val continue : ?key:Content.key -> t -> unit
  end

  module Cursor_with_value : sig
    include module type of Cursor

    val value : t -> Content.t option
    val delete : t -> unit Request.t
    val update : Content.t -> t -> Content.key Request.t
  end

  val add : Content.t -> ?key:Content.key -> t -> Content.key Request.t

  val open_cursor :
    ?query:Jv.t ->
    ?direction:Direction.t ->
    t ->
    Cursor_with_value.t option Request.t

  val put : Content.t -> ?key:Content.key -> t -> Content.key Request.t
end

module Make_object_store (C : Store_content_intf) = struct
  type t = Jv.t

  module Content = C

  module Cursor = struct
    type t = Jv.t

    external of_jv : Jv.t -> t = "%identity"

    let key t = Jv.get t "key" |> Jv.to_option Content.key_of_jv

    let advance count t =
      ignore @@ Jv.call t "advance" [| Jv.of_int count |];
      t

    let continue ?key t =
      let args =
        match key with None -> [||] | Some key -> [| Content.key_to_jv key |]
      in
      ignore @@ Jv.call t "continue" args
  end

  module Cursor_with_value = struct
    include Cursor

    let value t =
      let of_jv j = Content.of_jv j |> Result.get_ok in
      let v = Jv.get t "value" in
      Jv.to_option of_jv v

    let delete t = Jv.call t "delete" [||] |> Request.of_jv ~f:(fun _ -> ())

    let update v t =
      Jv.call t "update" [| Content.to_jv v |]
      |> Request.of_jv ~f:Content.key_of_jv
  end

  external of_jv : Jv.t -> t = "%identity"

  let add v ?(key : C.key option) t : C.key Request.t =
    let args =
      match key with
      | Some key -> [| C.to_jv v; C.key_to_jv key |]
      | None -> [| C.to_jv v |]
    in
    Jv.call t "add" args |> Request.of_jv ~f:C.key_of_jv

  let open_cursor ?query ?direction t : Cursor_with_value.t option Request.t =
    let direction = Option.map Direction.to_jv direction in
    let args =
      (* todo: query !*)
      match (query, direction) with
      | Some q, Some d -> [| q; d |]
      | None, Some d -> [| Jv.null; d |]
      | Some q, None -> [| q |]
      | None, None -> [||]
    in
    let f jv = Jv.to_option Cursor_with_value.of_jv jv in
    Jv.call t "openCursor" args |> Request.of_jv ~f

  let put v ?(key : C.key option) t : C.key Request.t =
    let args =
      match key with
      | Some key -> [| C.to_jv v; C.key_to_jv key |]
      | None -> [| C.to_jv v |]
    in
    Jv.call t "put" args |> Request.of_jv ~f:C.key_of_jv
end

module Transaction = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"

  type mode = Readonly | Readwrite | Readwriteflush

  let string_of_mode = function
    | Readonly -> "readonly"
    | Readwrite -> "readwrite"
    | Readwriteflush -> "readwriteflush"

  let mode_of_string = function
    | "readonly" -> Readonly
    | "readwrite" -> Readwrite
    | "readwriteflush" -> Readwriteflush
    | s -> raise (Invalid_argument s)

  let object_store (type t') (module S : Object_store_intf with type t = t') t :
      t' =
    Jv.call t "objectStore" [| Jv.of_string S.Content.name |] |> S.of_jv
end

module Database = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"

  let create_object_store (type t')
      (module S : Object_store_intf with type t = t') ?(auto_increment = false)
      (db : t) : t' =
    let opts = [ ("autoIncrement", Jv.of_bool auto_increment) ] in
    (* TODO: move autoincrement to store_content *)
    let opts = ("keyPath", Jv.of_string S.Content.key_path) :: opts in
    let options = Jv.obj @@ Array.of_list opts in
    (* TODO: keypath should be optionnal *)
    Jv.call db "createObjectStore" [| Jv.of_string S.Content.name; options |]
    |> S.of_jv

  let transaction stores ?(mode = Transaction.Readonly) t =
    let mode = Transaction.string_of_mode mode |> Jv.of_string in
    let jv_of_store (module S : Object_store_intf) =
      Jv.of_string S.Content.name
    in
    Jv.call t "transaction" [| Jv.of_list jv_of_store stores; mode |]
    |> Transaction.of_jv
end

module Open_db_request = struct
  type t = Database.t Request.t

  let on_upgrade_needed ~(f : Events.Version_change.t Ev.t -> t -> unit) (t : t)
      : t =
    let f ev =
      let req : t =
        Ev.current_target ev |> Ev.target_to_jv
        |> Request.of_jv ~f:Database.of_jv
      in
      f ev req
    in
    ignore @@ Ev.listen Events.upgrade_needed f (Ev.target_of_jv t.jv);
    t

  external as_request : t -> Database.t Request.t = "%identity"
end

module Factory = struct
  type t = Jv.t

  let open' ~name ?version t : Open_db_request.t =
    let args =
      match version with
      | Some v -> [| name; string_of_int v |]
      | None -> [| name |]
    in

    Jv.call t "open" @@ Array.map Jv.of_string args
    |> Request.of_jv ~f:Database.of_jv
end

let get_factory ?(global = Jv.global) () : Factory.t = Jv.get global "indexedDB"

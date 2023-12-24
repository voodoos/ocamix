open Brr

module type Store_intf = sig
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

module Object_store = struct
  type 'a t = Jv.t

  external of_jv : Jv.t -> 'a t = "%identity"

  let add (type t' key')
      (module S : Store_intf with type t = t' and type key = key') v
      ?(key : key' option) t : key' Request.t =
    let args =
      match key with
      | Some key -> [| S.to_jv v; S.key_to_jv key |]
      | None -> [| S.to_jv v |]
    in
    Jv.call t "add" args |> Request.of_jv ~f:S.key_of_jv
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

  let object_store (type t') (module S : Store_intf with type t = t') t :
      t' Object_store.t =
    Jv.call t "objectStore" [| Jv.of_string S.name |] |> Object_store.of_jv
end

module Database = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"

  let create_object_store (type t') (module S : Store_intf with type t = t')
      ?(auto_increment = false) t : t' Object_store.t =
    let opts = [ ("autoIncrement", Jv.of_bool auto_increment) ] in
    let opts = ("keyPath", Jv.of_string S.key_path) :: opts in
    let options = Jv.obj @@ Array.of_list opts in
    (* TODO: keypath should be optionnal *)
    Jv.call t "createObjectStore" [| Jv.of_string S.name; options |]
    |> Object_store.of_jv

  let transaction ~stores ?(mode = Transaction.Readonly) t =
    let mode = Transaction.string_of_mode mode |> Jv.of_string in
    Jv.call t "transaction" [| Jv.of_list Jv.of_string stores; mode |]
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

let get ?(window = G.window) () : Factory.t =
  Jv.get (Window.to_jv window) "indexedDB"

open Brr

module Key_path = struct
  type t = Identifier of string | Identifiers of string array

  let to_jv = function
    | Identifier p -> Jv.of_string p
    | Identifiers keys -> Jv.of_array Jv.of_string keys
end

module Key_range = struct
  type t = Jv.t

  external of_jv : Jv.t -> 'a = "%identity"

  let g = Jv.get Jv.global "IDBKeyRange"

  let bound ~lower ~upper ?(lower_open = false) ?(upper_open = false) () =
    Jv.call g "bound"
      [| lower; upper; Jv.of_bool lower_open; Jv.of_bool upper_open |]

  let only value = Jv.call g "only" [| value |]
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
  let error : Ev.Type.void Ev.type' = Ev.Type.void (Jstr.v "error")
end

module Request = struct
  type 'a t = { jv : Jv.t; of_jv : Jv.t -> 'a }

  external of_jv : Jv.t -> 'a = "%identity"

  let of_jv ~f j = { jv = of_jv j; of_jv = f }
  let error t = Jv.get t.jv "error" |> Jv.to_error

  let result (type a) (t : a t) : a =
    (* todo this is wrong *)
    Jv.get t.jv "result" |> t.of_jv

  let on_success (type a) ~(f : Ev.Type.void Ev.t -> a t -> unit) (t : a t) =
    let f ev = f ev t in
    ignore @@ Ev.listen Events.success f (Ev.target_of_jv t.jv);
    t

  let on_error (type a) ~(f : Ev.Type.void Ev.t -> a t -> unit) (t : a t) =
    let f ev = f ev t in
    ignore @@ Ev.listen Events.error f (Ev.target_of_jv t.jv);
    t

  let fut t =
    let result_fut, set = Fut.create () in
    let _ = on_success ~f:(fun _ t -> set (Ok (result t))) t in
    let _ = on_error ~f:(fun _ t -> set (Error (error t))) t in
    result_fut

  let fut_exn t =
    Fut.map
      (function
        | Ok v -> v
        | Error e ->
            Console.error [ "Request failed: "; e ];
            raise (Jv.Error e))
      (fut t)
end

module type Key = sig
  type t

  val to_jv : t -> Jv.t
  val of_jv : Jv.t -> t
end

module Auto_increment : Key with type t = int = struct
  type t = int

  let to_jv = Jv.of_int
  let of_jv = Jv.to_int
end

module type Store_content_intf = sig
  type t

  val name : string
  val to_jv : t -> Jv.t
  val of_jv : Jv.t -> t
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

module Content_access
    (Content : Store_content_intf)
    (Primary_key : Key)
    (Key : Key) =
struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"

  module Content = Content
  module Primary_key = Primary_key

  let count () t = Jv.call t "count" [||] |> Request.of_jv ~f:Jv.to_int

  let get key t =
    let f jv = Jv.to_option (fun j -> Content.of_jv j) jv in
    Jv.call t "get" [| Key.to_jv key |] |> Request.of_jv ~f

  let get_key key t =
    let f jv = Jv.to_option (fun j -> Primary_key.of_jv j) jv in
    Jv.call t "getKey" [| Key.to_jv key |] |> Request.of_jv ~f

  let get_all t =
    let f jv = Jv.to_array (fun c -> Content.of_jv c) jv in
    Jv.call t "getAll" [||] |> Request.of_jv ~f

  let get_all_keys ?query t =
    let args = match query with None -> [||] | Some query -> [| query |] in
    let f jv = Jv.to_array (fun c -> Primary_key.of_jv c) jv in
    Jv.call t "getAllKeys" args |> Request.of_jv ~f

  module Cursor = struct
    type t = Jv.t

    external of_jv : Jv.t -> t = "%identity"

    let key t = Jv.get t "key" |> Jv.to_option Key.of_jv
    let primary_key t = Jv.get t "primaryKey" |> Jv.to_option Primary_key.of_jv

    let advance count t =
      ignore @@ Jv.call t "advance" [| Jv.of_int count |];
      t

    let continue ?key t =
      let args =
        match key with None -> [||] | Some key -> [| Primary_key.to_jv key |]
      in
      ignore @@ Jv.call t "continue" args
  end

  module Cursor_with_value = struct
    include Cursor

    let value t =
      let of_jv j = Content.of_jv j in
      let v = Jv.get t "value" in
      Jv.to_option of_jv v

    let delete t = Jv.call t "delete" [||] |> Request.of_jv ~f:(fun _ -> ())

    let update v t =
      Jv.call t "update" [| Content.to_jv v |]
      |> Request.of_jv ~f:Primary_key.of_jv
  end

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

  let open_key_cursor ?query ?direction t : Cursor_with_value.t option Request.t
      =
    let direction = Option.map Direction.to_jv direction in
    let args =
      (* todo: query !*)
      match (query, direction) with
      | Some q, Some d -> [| q; d |]
      | None, Some d -> [| Jv.null; d |]
      | Some q, None -> [| q |]
      | None, None -> [||]
    in
    let f jv = Jv.to_option Cursor.of_jv jv in
    Jv.call t "openKeyCursor" args |> Request.of_jv ~f

  (* [fold_key] will fold over all keys returned by the given cursor.
     Note that it is less efficient than [get_all_keys] to build an array
     of every keys. It's most probably due to a caching-based optimization
     of the [get_all_keys] results (at least in Firefox). *)
  let fold_keys ~init ~f cursor_req =
    let result, set_result = Fut.create () in
    let acc = ref init in
    let _ =
      Request.on_success cursor_req ~f:(fun _ev r ->
          match Request.result r with
          | None -> set_result (Ok !acc)
          | Some cursor ->
              (* The cursor should not be out of range at that point so the keys
                 should have a value. *)
              let key = Cursor.key cursor |> Option.get in
              let primary_key = Cursor.primary_key cursor |> Option.get in
              acc := f !acc key primary_key;
              Cursor.continue cursor)
      |> Request.on_error ~f:(fun _ev req ->
             set_result (Error (Request.error req)))
    in
    result
end

module type Index_intf = sig
  type t

  val of_jv : Jv.t -> t

  module Key : Key
end

module type Store_intf = sig
  type t

  val of_jv : Jv.t -> t

  module Content : Store_content_intf
  module Primary_key : Key

  val add : Content.t -> ?key:Primary_key.t -> t -> Primary_key.t Request.t

  val create_index :
    (module Index_intf with type t = 't) ->
    name:string ->
    Key_path.t ->
    ?unique:bool ->
    t ->
    't

  val index : (module Index_intf with type t = 't) -> name:string -> t -> 't
  val put : Content.t -> ?key:Primary_key.t -> t -> Primary_key.t Request.t
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

  let object_store (type t') (module S : Store_intf with type t = t') t : t' =
    Jv.call t "objectStore" [| Jv.of_string S.Content.name |] |> S.of_jv

  let commit t = Jv.call t "commit" [||] |> ignore
end

module Database = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"

  let create_object_store (type t') (module S : Store_intf with type t = t')
      ?key_path ?(auto_increment = false) (db : t) : t' =
    let opts = [ ("autoIncrement", Jv.of_bool auto_increment) ] in
    let opts =
      match key_path with
      | None -> opts
      | Some key_path ->
          let key_path = Key_path.to_jv key_path in
          ("keyPath", key_path) :: opts
    in
    let options = Jv.obj @@ Array.of_list opts in
    Console.info [ "new object store with options:"; options ];
    Jv.call db "createObjectStore" [| Jv.of_string S.Content.name; options |]
    |> S.of_jv

  let delete_object_store t name =
    Jv.call t "deleteObjectStore" [| Jv.of_string name |] |> ignore

  let transaction stores ?(mode = Transaction.Readonly) t =
    let mode = Transaction.string_of_mode mode |> Jv.of_string in
    let jv_of_store (module S : Store_intf) = Jv.of_string S.Content.name in
    Jv.call t "transaction" [| Jv.of_list jv_of_store stores; mode |]
    |> Transaction.of_jv

  let object_store_names t =
    Jv.get t "objectStoreNames" |> Jv.to_array Jv.to_string
end

module Make_object_store (Content : Store_content_intf) (Primary_key : Key) =
struct
  module Store = struct
    include Content_access (Content) (Primary_key) (Primary_key)

    let add v ?(key : Primary_key.t option) t : Primary_key.t Request.t =
      let args =
        match key with
        | Some key -> [| Content.to_jv v; Primary_key.to_jv key |]
        | None -> [| Content.to_jv v |]
      in
      Jv.call t "add" args |> Request.of_jv ~f:Primary_key.of_jv

    let create_index (type t') (module I : Index_intf with type t = t') ~name
        key_path ?unique t : t' =
      let options =
        [ Option.map (fun b -> ("unique", Jv.of_bool b)) unique ]
        |> List.filter_map Fun.id |> Array.of_list |> Jv.obj
      in

      let key_path = Key_path.to_jv key_path in
      Jv.call t "createIndex" [| Jv.of_string name; key_path; options |]
      |> I.of_jv

    let index (type t') (module I : Index_intf with type t = t') ~name t : t' =
      Jv.call t "index" [| Jv.of_string name |] |> I.of_jv

    let put v ?(key : Primary_key.t option) t : Primary_key.t Request.t =
      let args =
        match key with
        | Some key -> [| Content.to_jv v; Primary_key.to_jv key |]
        | None -> [| Content.to_jv v |]
      in
      Jv.call t "put" args |> Request.of_jv ~f:Primary_key.of_jv
  end

  include Store

  let create ?key_path ?auto_increment db =
    Database.create_object_store (module Store) ?key_path ?auto_increment db
end

module Make_index (Store : Store_intf) (Key : Key) = struct
  module Index = struct
    include Content_access (Store.Content) (Store.Primary_key) (Key)
    module Key = Key
  end

  include Index

  let create ~name key_path ?unique store =
    Store.create_index (module Index) ~name key_path ?unique store
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

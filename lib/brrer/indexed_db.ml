open Brr

module type Store_content = sig
  type t
  type key

  val to_jv : t -> Jv.t
  val of_jv : Jv.t -> (t, [ `Msg of string ]) Result.t
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

module Object_store = struct
  type 'a t = Jv.t

  external of_jv : Jv.t -> 'a t = "%identity"
end

module Database = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"

  let create_object_store (type t') ~name
      (module S : Store_content with type t = t') ?(auto_increment = false) t :
      t' Object_store.t =
    let opts = [ ("autoIncrement", Jv.of_bool auto_increment) ] in
    let opts = ("keyPath", Jv.of_string S.key_path) :: opts in
    let options = Jv.obj @@ Array.of_list opts in
    Jv.call t "createObjectStore" [| Jv.of_string name; options |]
    |> Object_store.of_jv
end

module Request = struct
  type 'a t = Jv.t

  external of_jv : Jv.t -> 'a t = "%identity"
  external to_a : 'a t -> 'a = "%identity"

  let result (type a) (t : a t) : a = Jv.get t "result" |> to_a

  let on_success (type a) ~(f : Ev.Type.void Ev.t -> a t -> unit) (t : a t) =
    let f ev =
      let req : 'a t = Ev.current_target ev |> Ev.target_to_jv |> of_jv in
      f ev req
    in
    ignore @@ Ev.listen Events.success f (Ev.target_of_jv t);
    t
end

module Open_db_request = struct
  type t = Database.t Request.t

  let on_upgrade_needed ~(f : Events.Version_change.t Ev.t -> t -> unit) t =
    let f ev =
      let req : t = Ev.current_target ev |> Ev.target_to_jv |> Request.of_jv in
      f ev req
    in
    ignore @@ Ev.listen Events.upgrade_needed f (Ev.target_of_jv t);
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
end

let get ?(window = G.window) () : Factory.t =
  Jv.get (Window.to_jv window) "indexedDB"

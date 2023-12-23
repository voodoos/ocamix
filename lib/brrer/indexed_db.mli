open Brr

module type Store_content = sig
  type t
  type key

  val to_jv : t -> Jv.t
  val of_jv : Jv.t -> (t, [ `Msg of string ]) Result.t
  val key_path : string
  val get_key : t -> key
end

module Events : sig
  module Version_change : sig
    type t

    val old_version : t -> int
    val new_version : t -> int
  end

  val upgrade_needed : Version_change.t Ev.type'
  val success : Ev.Type.void Ev.type'
end

module Object_store : sig
  type 'a t

  val of_jv : Jv.t -> 'a t
end

module Database : sig
  type t

  val of_jv : Jv.t -> t

  val create_object_store :
    name:string ->
    (module Store_content with type t = 't) ->
    ?auto_increment:bool ->
    t ->
    't Object_store.t
end

module Request : sig
  type 'a t

  val result : 'a t -> 'a
  val on_success : f:(Ev.Type.void Ev.t -> 'a t -> unit) -> 'a t -> 'a t
end

module Open_db_request : sig
  type t = Database.t Request.t

  val on_upgrade_needed :
    f:(Events.Version_change.t Brr.Ev.t -> t -> unit) -> t -> t

  val as_request : t -> Database.t Request.t
end

module Factory : sig
  type t

  val open' : name:string -> ?version:int -> t -> Open_db_request.t
end

val get : ?window:Brr.El.window -> unit -> Factory.t

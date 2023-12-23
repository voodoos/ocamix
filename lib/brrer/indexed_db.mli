open Brr

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
  type t

  val of_jv : Jv.t -> t
end

module Database : sig
  type t

  val of_jv : Jv.t -> t

  val create_object_store :
    name:string ->
    ?key_path:string ->
    ?auto_increment:bool ->
    t ->
    Object_store.t
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

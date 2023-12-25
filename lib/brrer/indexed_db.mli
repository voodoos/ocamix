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

module Direction : sig
  type t = Next | Next_unique | Prev | Prev_unique

  val to_string : t -> string
  val of_string : string -> t
  val to_jv : t -> Jv.t
  val of_jv : Jv.t -> t
end

module Request : sig
  type 'a t

  val result : 'a t -> 'a
  val on_success : f:(Ev.Type.void Ev.t -> 'a t -> unit) -> 'a t -> 'a t
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

module type Object_store_intf = sig
  type t

  val of_jv : Jv.t -> t

  module Content : Store_content_intf

  module Cursor : sig
    type t

    val key : t -> Content.key option
  end

  module Cursor_with_value : sig
    include module type of Cursor

    val value : t -> Content.t option
  end

  val add : Content.t -> ?key:Content.key -> t -> Content.key Request.t

  val open_cursor :
    ?query:Jv.t ->
    ?direction:Direction.t ->
    t ->
    Cursor_with_value.t option Request.t

  val put : Content.t -> ?key:Content.key -> t -> Content.key Request.t
end

module Make_object_store (C : Store_content_intf) : sig
  include Object_store_intf with module Content = C
end

module Transaction : sig
  type t
  type mode = Readonly | Readwrite | Readwriteflush

  val string_of_mode : mode -> string
  val mode_of_string : string -> mode
  val object_store : (module Object_store_intf with type t = 't) -> t -> 't
end

module Database : sig
  type t

  val of_jv : Jv.t -> t

  val create_object_store :
    (module Object_store_intf with type t = 't) ->
    ?auto_increment:bool ->
    t ->
    't

  val transaction :
    (module Object_store_intf) list ->
    ?mode:Transaction.mode ->
    t ->
    Transaction.t
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

val get_factory : ?window:Brr.El.window -> unit -> Factory.t

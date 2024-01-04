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

  val error : 'a t -> Jv.Error.t
  val result : 'a t -> 'a
  val on_success : f:(Ev.Type.void Ev.t -> 'a t -> unit) -> 'a t -> 'a t
  val on_error : f:(Ev.Type.void Ev.t -> 'a t -> unit) -> 'a t -> 'a t
  val fut : 'a t -> ('a, [> `Jv of Jv.Error.t ]) Fut.result
end

module type Key = sig
  type t

  val path : string (* todo key_path should be optional *)
  val to_jv : t -> Jv.t
  val of_jv : Jv.t -> t
end

module type Store_content_intf = sig
  type t

  module Key : Key

  val name : string
  val to_jv : t -> Jv.t
  val of_jv : Jv.t -> t
  val get_key : t -> Key.t
end

module Content_access (Content : Store_content_intf) (Key : Key) : sig
  type t

  external of_jv : Jv.t -> t = "%identity"

  val count : unit -> t -> int Request.t
  (** TODO: count has optional parameters *)

  val get : Key.t -> t -> Content.t option Request.t
  val get_all : t -> Content.t Array.t Request.t
  (* TODO: [get_all] optional parameters *)

  module Cursor : sig
    type t

    val key : t -> Key.t option
    val primary_key : t -> Content.Key.t option
    val advance : int -> t -> t

    val continue : ?key:Content.Key.t -> t -> unit
    (** [continue t] advances the cursor to the next position along its
      direction, to the item whose key matches the optional key parameter. This
      will re-trigger the [on_success] event of the cursor's query. *)
  end

  module Cursor_with_value : sig
    include module type of Cursor

    val value : t -> Content.t option
    val delete : t -> unit Request.t
    val update : Content.t -> t -> Content.Key.t Request.t
  end

  val open_cursor :
    ?query:Jv.t ->
    ?direction:Direction.t ->
    t ->
    Cursor_with_value.t option Request.t
end

module type Store = sig
  type t

  val of_jv : Jv.t -> t

  module Content : Store_content_intf
end

module type Index = sig
  type t

  val of_jv : Jv.t -> t
  val name : string

  module Key : Key
end

module Make_index
    (P : sig
      val name : string
    end)
    (C : Store_content_intf)
    (Key : Key) : sig
  module Content : Store_content_intf with type t = C.t and type Key.t = C.Key.t
  module Key : Key with type t = Key.t
  include module type of Content_access (Content) (Key)
  include module type of P
end

module Make_object_store (C : Store_content_intf) : sig
  module Content : Store_content_intf with type t = C.t and type Key.t = C.Key.t
  include module type of Content_access (Content) (Content.Key)

  val add : Content.t -> ?key:Content.Key.t -> t -> Content.Key.t Request.t
  val create_index : (module Index with type t = 't) -> t -> 't
  val index : (module Index with type t = 't) -> t -> 't
  val put : Content.t -> ?key:Content.Key.t -> t -> Content.Key.t Request.t
end

module Transaction : sig
  type t
  type mode = Readonly | Readwrite | Readwriteflush

  val string_of_mode : mode -> string
  val mode_of_string : string -> mode
  val object_store : (module Store with type t = 't) -> t -> 't
end

module Database : sig
  type t

  val of_jv : Jv.t -> t

  val create_object_store :
    (module Store with type t = 't) -> ?auto_increment:bool -> t -> 't

  val transaction :
    (module Store) list -> ?mode:Transaction.mode -> t -> Transaction.t
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

val get_factory : ?global:Jv.t -> unit -> Factory.t
(** Returns a [Database] factory. In some browsers it might be necessary to
    specify a global object (like [window]) with the [indexedDb] property. *)

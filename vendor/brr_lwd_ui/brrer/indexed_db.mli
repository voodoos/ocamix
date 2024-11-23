open Brr

(* TODO we don't need all those functors ! *)

module Key_path : sig
  type t = Identifier of string | Identifiers of string array
end

module Key_range : sig
  type t

  val of_jv : Jv.t -> t

  val bound :
    lower:Jv.t ->
    upper:Jv.t ->
    ?lower_open:bool ->
    ?upper_open:bool ->
    unit ->
    t

  val only : Jv.t -> t
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
  val fut : 'a t -> 'a Fut.or_error
  val fut_exn : 'a t -> 'a Fut.t
end

module type Key = sig
  type t
  (* TODO keys are not arbitrary javascript values. Keys can be: string, date, float, a binary blob, and array. For
     arrays, the key can range from an empty value to infinity. And you can include
     an array within an array. *)

  val to_jv : t -> Jv.t
  val of_jv : Jv.t -> t
end

module Auto_increment : Key with type t = int

module type Store_content_intf = sig
  type t

  val name : string
  val to_jv : t -> Jv.t
  val of_jv : Jv.t -> t
end

module Content_access
    (Content : Store_content_intf)
    (Primary_key : Key)
    (Key : Key) : sig
  type t

  val of_jv : Jv.t -> t

  module Content : Store_content_intf with type t = Content.t
  module Primary_key : Key with type t = Primary_key.t

  module Cursor : sig
    type t

    val key : t -> Key.t option
    val primary_key : t -> Primary_key.t option
    val advance : int -> t -> t

    val continue : ?key:Primary_key.t -> t -> unit
    (** [continue t] advances the cursor to the next position along its
        direction, to the item whose key matches the optional key parameter.
        This will re-trigger the [on_success] event of the cursor's query. *)
  end

  module Cursor_with_value : sig
    include module type of Cursor

    val value : t -> Content.t option
    val delete : t -> unit Request.t
    val update : Content.t -> t -> Primary_key.t Request.t
  end

  val count : unit -> t -> int Request.t
  (** TODO: count has optional parameters *)

  val get : Key.t -> t -> Content.t option Request.t
  val get_key : Key.t -> t -> Primary_key.t option Request.t
  (* TODO: other parameters are possible *)

  val get_all : t -> Content.t Array.t Request.t
  (** [get_all] retrieves all objects that are inside the index. There is a
      performance cost associated with looking at the value property of a
      cursor, because the object is created lazily and [get_all] force the
      cration of all objects in the store.

      TODO: optional parameters *)

  val get_all_keys : ?query:Key_range.t -> t -> Primary_key.t Array.t Request.t
  (** [get_all_keys] retrieves the list of all primary keys.

      TODO stronger query type TODO optional count parameter *)

  val fold_keys :
    init:'a ->
    f:('a -> Key.t -> Primary_key.t -> 'a) ->
    Cursor.t option Request.t ->
    'a Fut.or_error
  (** [fold_keys c] enable convenient iteration other the keys of an index. It
      is slower than [get_all_keys] but is not restricted to the primary key.
      This utility function is specific to the bindings and implemented by
      iterating other the key_cursor opened with by given request. *)

  val open_cursor :
    ?query:Jv.t ->
    ?direction:Direction.t ->
    t ->
    Cursor_with_value.t option Request.t

  val open_key_cursor :
    ?query:Jv.t -> ?direction:Direction.t -> t -> Cursor.t option Request.t
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

module Transaction : sig
  type t
  type mode = Readonly | Readwrite | Readwriteflush

  val string_of_mode : mode -> string
  val mode_of_string : string -> mode
  val object_store : (module Store_intf with type t = 't) -> t -> 't
  val commit : t -> unit
end

module Database : sig
  type t

  val of_jv : Jv.t -> t

  val create_object_store :
    (module Store_intf with type t = 't) ->
    ?key_path:Key_path.t ->
    ?auto_increment:bool ->
    t ->
    't
  (** Creates and returns a new object store or index. *)

  val delete_object_store : t -> string -> unit

  val transaction :
    (module Store_intf) list -> ?mode:Transaction.mode -> t -> Transaction.t

  val object_store_names : t -> string array
end

module Make_object_store
    (Content : Store_content_intf)
    (* TODO: at that step do we really need the content's type ?*)
    (Primary_key : Key) : sig
  include module type of Content_access (Content) (Primary_key) (Primary_key)

  include
    Store_intf
      with type t := t
       and type Content.t = Content.t
       and type Primary_key.t = Primary_key.t

  val create : ?key_path:Key_path.t -> ?auto_increment:bool -> Database.t -> t
  (** Creates and returns a new object store or index. [Some_store.create db] is
      equivalent to [Database.create_object_store (module Some_store) db] but
      more convenient to use in most cases. *)
end

module Make_index (Store : Store_intf) (Key : Key) : sig
  module Key : Key with type t = Key.t

  include module type of
      Content_access (Store.Content) (Store.Primary_key) (Key)

  val create : name:string -> Key_path.t -> ?unique:bool -> Store.t -> t
  (** Creates a new index during a version upgrade. [Some_index.create store] is
      equivalent to [Store.create_index (module Some_index) store] but more
      convenient to use in most cases. *)
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

(* Note about key comparisons:

   https://www.w3.org/TR/IndexedDB/#compare-two-keys

   To compare two keys a and b, run these steps:

     1. Let ta be the type of a.
     2. Let tb be the type of b.
     3. If ta does not equal tb, then run these steps:
       1. If ta is array, then return 1.
       2. If tb is array, then return -1.
       3. If ta is binary, then return 1.
       4. If tb is binary, then return -1.
       5. If ta is string, then return 1.
       6. If tb is string, then return -1.
       7. If ta is date, then return 1.
       8. Assert: tb is date.
       9. Return -1.

     4. Let va be the value of a.
     5. Let vb be the value of b.
     6.  Switch on ta:
       - number
       - date
         1. If va is greater than vb, then return 1.
         2. If va is less than vb, then return -1.
         3. Return 0.
       - string
         1. If va is code unit less than vb, then return -1.
         2. If vb is code unit less than va, then return 1.
         3. Return 0.
       - binary
         1. If va is byte less than vb, then return -1.
         2. If vb is byte less than va, then return 1.
         3. Return 0.
       - array
         1. Let length be the lesser of va’s size and vb’s size.
         2. Let i be 0.
         3. While i is less than length, then:
           1. Let c be the result of recursively comparing two keys with va[i] and vb[i].
           2. If c is not 0, return c.
           3. Increase i by 1.
         4. If va’s size is greater than vb’s size, then return 1.
         5. If va’s size is less than vb’s size, then return -1.
         6. Return 0.

   The key a is greater than the key b if the result of comparing two keys with a and b is 1.

   The key a is less than the key b if the result of comparing two keys with a and b is -1.

   The key a is equal to the key b if the result of comparing two keys with a and b is 0.*)

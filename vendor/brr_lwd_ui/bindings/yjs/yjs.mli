type observer

module StringMap : module type of Map.Make (String)

module Event : sig
  type 'a t
  type 'a changes = { delta : 'a array }

  val changes : 'change t -> 'change changes
end

module rec Array : sig
  type t
  type value = [ `Jv of Jv.t | `Map of Map.t | `Array of Array.t ]
  type change = Retain of int | Insert of value array | Delete of int

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"
  val make : unit -> t
  val insert : t -> int -> value array -> unit
  val iter : t -> f:(index:int -> value -> t -> unit) -> unit
  val observe : t -> (change Event.t -> unit) -> observer
end

and Map : sig
  type t
  type value = [ `Jv of Jv.t | `Map of Map.t | `Array of Array.t ]

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

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"
  val make : unit -> t
  val get : t -> key:string -> value option
  val set : t -> key:string -> value -> unit
  val delete : t -> string -> unit
  val entries : t -> Jv.It.t

  val fold_entries :
    t -> f:(string -> value -> 'acc -> 'acc) -> init:'acc -> 'acc

  val observe : t -> (Event.t -> unit) -> observer
end

module Doc : sig
  type t

  external of_jv : Jv.t -> 'a = "%identity"
  val make : unit -> t
  val get_array : t -> string -> Array.t
  val get_map : t -> string -> Map.t
end

val global : Jv.t

type observer

module Event : sig
  type t

  external of_jv : Jv.t -> t = "%identity"

  type changes = { delta : Jv.t }

  val changes : t -> changes
end

module Array : sig
  type t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"
  val insert : t -> int -> Jv.t array -> unit
  val observe : t -> (Event.t -> unit) -> observer
end

module Map : sig
  type t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"
  val get : t -> key:string -> Jv.t (* TODO: this could be tighter *)
  val set : t -> key:string -> Jv.t -> unit
  val entries : t -> Jv.It.t
  val observe : t -> (Event.t -> unit) -> observer
end

module Doc : sig
  type t

  external of_jv : Jv.t -> 'a = "%identity"
  val make : unit -> t
  val get_array : t -> string -> Array.t
  val get_map : t -> string -> Map.t
end

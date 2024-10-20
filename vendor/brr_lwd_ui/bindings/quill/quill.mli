type t

val to_jv : t -> Jv.t
val register : path:string -> Jv.t -> unit
val cursors : Jv.t

type config
type theme = Snow | Bubble

val config : ?theme:theme -> ?cursors:bool -> unit -> config
val make : container:Brr.El.t -> config -> t

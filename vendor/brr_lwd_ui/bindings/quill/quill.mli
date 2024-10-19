type t

val to_jv : t -> Jv.t
val register : path:string -> Jv.t -> unit

type config
type theme = Snow | Bubble

val config : ?theme:theme -> unit -> config
val make : container:Brr.El.t -> config -> t

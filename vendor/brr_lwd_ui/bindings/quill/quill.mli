type t

val to_jv : t -> Jv.t
val register : path:string -> Jv.t -> unit
val cursors : Jv.t

type tool = Bold | Italic | Underline | Strike | Link
type toolbar = Array of tool list
type config
type theme = Snow | Bubble

val config : ?theme:theme -> ?cursors:bool -> ?toolbar:toolbar -> unit -> config
val make : container:Brr.El.t -> config -> t

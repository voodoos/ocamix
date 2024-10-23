type t

val to_jv : t -> Jv.t
val make : Doc.Doc.t -> t
val set_user_info : t -> name:string -> ?color:string -> unit -> unit

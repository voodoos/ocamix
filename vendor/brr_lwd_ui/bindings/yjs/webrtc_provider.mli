type t

val make : room_name:string -> ?signaling:string list -> Doc.Doc.t -> t

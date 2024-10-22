type t

val make :
  room_name:string ->
  ?signaling:string list ->
  ?awareness:Awareness.t ->
  Doc.Doc.t ->
  t

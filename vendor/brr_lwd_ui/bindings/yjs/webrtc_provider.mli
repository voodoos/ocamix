type t

val make :
  room_name:string ->
  ?signaling:string list ->
  ?awareness:Awareness.t ->
  Doc.Doc.t ->
  t

type status = { connected : bool }
type synced = { synced : bool }

type peers = {
  added : string list;
  removed : string list;
  webrtc_peers : string list;
  bc_peers : string list;
}

type 'a event =
  | Status : status event
  | Synced : synced event
  | Peers : peers event

val on : 'a. t -> 'a event -> f:('a -> unit) -> unit

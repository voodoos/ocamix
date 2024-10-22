type t = Jv.t

external get_awareness_protocol : unit -> Jv.t = "get_awareness_protocol"
external to_jv : t -> Jv.t = "%identity"

let class_awarness = Jv.get (get_awareness_protocol ()) "Awareness"
let make doc = Jv.new' class_awarness [| Doc.Doc.to_jv doc |]

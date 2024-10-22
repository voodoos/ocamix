type t = Jv.t

external to_jv : t -> Jv.t = "%identity"

let class_awarness = Jv.get Global.awareness_protocol "Awareness"
let make doc = Jv.new' class_awarness [| Doc.Doc.to_jv doc |]

open Brr

type t = Jv.t

include Jv.Id

let interface = Jv.get Jv.global "EventSource"
let as_target t = Ev.target_of_jv t
let create ~url () = Jv.new' interface [| Jv.of_jstr url |]

type t = Jv.t

external get_awareness_protocol : unit -> Jv.t = "get_awareness_protocol"
external to_jv : t -> Jv.t = "%identity"

let class_awarness = Jv.get (get_awareness_protocol ()) "Awareness"
let make doc = Jv.new' class_awarness [| Doc.Doc.to_jv doc |]

let set_local_state_field t ~field value =
  Jv.call t "setLocalStateField" [| Jv.of_string field; value |] |> ignore

let set_user_info t ~name ?color () =
  let info =
    Jv.obj
    @@
    match color with
    | Some color ->
        [| ("name", Jv.of_string name); ("color", Jv.of_string color) |]
    | None -> [| ("name", Jv.of_string name) |]
  in
  set_local_state_field t ~field:"user" info

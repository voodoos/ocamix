type t = Jv.t

let web_rpc_provider = Jv.get Jv.global "WebrtcProvider"

let make ~room_name ?signaling yjs_doc =
  let signaling =
    Option.map (fun v -> ("signaling", Jv.of_list Jv.of_string v)) signaling
  in
  let options = [ signaling ] |> List.filter_map Fun.id |> Array.of_list in
  Jv.new' web_rpc_provider
    [| Jv.of_string room_name; Doc.Doc.to_jv yjs_doc; Jv.obj options |]

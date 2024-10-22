type t = Jv.t

let quill_binding = Global.quill_binding

let make ?awareness text editor =
  let awareness = Option.map Yjs.Awareness.to_jv awareness in
  let params =
    Yjs.Text.to_jv text :: Quill.to_jv editor :: Option.to_list awareness
  in
  Jv.new' quill_binding @@ Array.of_list params

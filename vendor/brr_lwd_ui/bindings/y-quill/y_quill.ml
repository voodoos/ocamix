type t = Jv.t

let quill_binding = Global.quill_binding

let make text editor =
  Jv.new' quill_binding [| Yjs.Text.to_jv text; Quill.to_jv editor |]

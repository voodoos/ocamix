type t = Jv.t

external to_jv : t -> Jv.t = "%identity"

let quill = Global.quill
let cursors = Global.quill_cursors

let register ~path v =
  ignore @@ Jv.call quill "register" [| Jv.of_string path; v |]

type config = Jv.t
type theme = Snow | Bubble

let config ?(theme = Snow) ?cursors () : config =
  let theme = match theme with Snow -> "snow" | Bubble -> "bubble" in
  let cursors =
    match cursors with Some b -> [| ("cursors", Jv.of_bool b) |] | None -> [||]
  in
  let modules = Jv.obj cursors in
  Jv.obj [| ("theme", Jv.of_string theme); ("modules", modules) |]

let make ~container config = Jv.new' quill [| Brr.El.to_jv container; config |]

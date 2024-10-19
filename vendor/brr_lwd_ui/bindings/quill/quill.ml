let quill = Global.quill
let register ~path v = Jv.call quill "register" [| Jv.of_string path; v |]

type config = Jv.t
type theme = Snow | Bubble

let make_config ?(theme = Snow) () : config =
  let theme = match theme with Snow -> "snow" | Bubble -> "bubble" in
  Jv.obj [| ("theme", Jv.of_string theme) |]

let make ~container config = Jv.new' quill [| Brr.El.to_jv container; config |]

type t = Jv.t

external to_jv : t -> Jv.t = "%identity"

let quill = Global.quill
let cursors = Global.quill_cursors

let register ~path v =
  ignore @@ Jv.call quill "register" [| Jv.of_string path; v |]

type config = Jv.t
type theme = Snow | Bubble
type tool = Bold | Italic | Underline | Strike | Link

type toolbar =
  | Array of tool list (* TODO groups / type for values Bold | Italic... *)

let jv_of_tool t =
  Jv.of_string
  @@
  match t with
  | Bold -> "bold"
  | Italic -> "italic"
  | Underline -> "underline"
  | Strike -> "strike"
  | Link -> "link"

let config ?(theme = Snow) ?cursors ?toolbar () : config =
  let theme = match theme with Snow -> "snow" | Bubble -> "bubble" in
  let cursors = Option.map (fun b -> ("cursors", Jv.of_bool b)) cursors in
  let toolbar =
    Option.map
      (function Array t -> ("toolbar", Jv.of_list jv_of_tool t))
      toolbar
  in

  let modules =
    Jv.obj (List.filter_map Fun.id [ cursors; toolbar ] |> Array.of_list)
  in
  Jv.obj [| ("theme", Jv.of_string theme); ("modules", modules) |]

let make ~container config = Jv.new' quill [| Brr.El.to_jv container; config |]

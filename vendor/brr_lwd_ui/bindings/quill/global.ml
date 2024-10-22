external get_quill : unit -> Jv.t = "get_quill"
external get_quill_cursors : unit -> Jv.t = "get_quill_cursors"

let quill = get_quill ()
let quill_cursors = get_quill_cursors ()

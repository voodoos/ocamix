type t = Jv.t

external get_indexeddb_persistence : unit -> Jv.t = "get_indexeddb_persistence"

let indexeddb_persistence = get_indexeddb_persistence ()

let make ~doc_name doc =
  Jv.new' indexeddb_persistence [| Jv.of_string doc_name; Doc.Doc.to_jv doc |]

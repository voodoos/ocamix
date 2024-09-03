type t = Jv.t

let indexeddb_persistence = Jv.get Jv.global "IndexeddbPersistence"

let make ~doc_name doc =
  Jv.new' indexeddb_persistence [| Jv.of_string doc_name; Doc.Doc.to_jv doc |]

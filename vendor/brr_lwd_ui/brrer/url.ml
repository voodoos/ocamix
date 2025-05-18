open Brr

type t = Jv.t

let url = Jv.get Jv.global "URL"

let create_object_url (blob : Blob.t) =
  Jv.call url "createObjectURL" [| Blob.to_jv blob |] |> Jv.to_string

let revoke_object_url object_url =
  Jv.call url "revokeObjectURL" [| Jv.of_string object_url |] |> ignore

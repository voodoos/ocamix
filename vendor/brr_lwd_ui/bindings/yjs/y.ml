open Brr
module Doc = Doc.Doc

let encode_state_as_update doc : Tarray.uint8 =
  Jv.call Global.yjs "encodeStateAsUpdate" [| Doc.to_jv doc |] |> Tarray.of_jv

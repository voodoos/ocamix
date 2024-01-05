open Brr

type t = Jv.t

external of_jv : Jv.t -> t = "%identity"

module Mutation_record = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"

  let type' t = Jv.get t "type" |> Jv.to_string
end

(* todo options*)
let observe t target =
  let options = Jv.obj [| ("childList", Jv.true') |] in
  Jv.call t "observe" [| El.to_jv target; options |] |> ignore

let create ~callback () =
  let callback entries observer =
    let mutrecord = Jv.to_list Mutation_record.of_jv entries in
    callback mutrecord (of_jv observer)
  in
  let args = [| Jv.repr callback |] in
  Jv.new' (Jv.get Jv.global "MutationObserver") args

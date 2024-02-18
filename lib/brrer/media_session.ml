open Brr

type t = Jv.t

external of_jv : Jv.t -> t = "%identity"

let of_navigator n = Jv.get (Navigator.to_jv n) "mediaSession"

module Action = struct
  type t = Jstr.t

  let next_track = Jstr.v "nexttrack"
end

let set_action_handler t action f =
  let callback = Jv.callback ~arity:1 f in
  Jv.call t "setActionHandler" [| Jv.of_jstr action; callback |] |> ignore

include ContainersLabels

module String = struct
  include String
  module Set = Set.Make (String)
end

module Yojson = struct
  include Jsonxt.Yojson
end

module Encodings = struct
  let to_jstr t = Jv.repr t |> Brr.Json.encode
  let to_jv t = to_jstr t |> Jv.of_jstr

  let of_jstr jstr =
    match Brr.Json.decode jstr with
    | Ok v -> Ok (Obj.magic v)
    | Error err ->
        Brr.Console.error [ err ];
        Error (`Msg "Failed to unmarshal data")

  let of_jv jv = Jv.to_jstr jv |> of_jstr
end

let random_state = Random.get_state ()
let new_uuid_v4 () = Uuidm.v4_gen random_state ()

include ContainersLabels

module String = struct
  include String
  module Set = Set.Make (String)
end

module Yojson = struct
  include Jsonxt.Yojson
end

module Encodings = struct
  let marshal_to_jstr t = Marshal.to_string t [] |> Jstr.binary_of_octets
  let marshal_to_jv t = marshal_to_jstr t |> Jv.of_jstr

  let unmarshal_jstr jstr =
    try Ok (Marshal.from_string (Jstr.binary_to_octets jstr) 0)
    with _ -> Error (`Msg "Failed to unmarshal data")

  let unmarshal_jv jv = Jv.to_jstr jv |> unmarshal_jstr
end

let random_state = Random.get_state ()
let new_uuid_v4 () = Uuidm.v4_gen random_state ()

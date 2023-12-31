include ContainersLabels

module String = struct
  include String
  module Set = Set.Make (String)
end

module Yojson = struct
  include Jsonxt.Yojson
end

module Encodings = struct
  let marshal_to_jstr t =
    Marshal.to_string t [] |> Base64.encode_string |> Jstr.v

  let marshal_to_jv t = marshal_to_jstr t |> Jv.of_jstr

  let unmarshal_jstr jstr =
    let open Result.Infix in
    try
      let+ str = Jstr.to_string jstr |> Base64.decode in
      Marshal.from_string str 0
    with _ -> Error (`Msg "Failed to unmarshal data")

  let unmarshal_jv jv = Jv.to_jstr jv |> unmarshal_jstr
end

include ContainersLabels

module String = struct
  include String
  module Set = Set.Make (String)
end

module Json = struct
  let to_string v =
    match
      Json_repr.pp ~compact:true
        (module Json_repr.Yojson)
        Format.str_formatter v
    with
    | () ->
        let v = Format.flush_str_formatter () in
        v

  let from_string s = Ezjsonm.from_string s |> Json_repr.to_yojson
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

(** [tee f x] applies [f] to [x] and returns [x] *)
let tee f x =
  let () = f x in
  x

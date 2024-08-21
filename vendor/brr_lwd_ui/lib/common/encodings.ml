open Import

let to_jstr t = Jv.repr t |> Brr.Json.encode
let to_jv t = to_jstr t |> Jv.of_jstr

let of_jstr jstr =
  match Brr.Json.decode jstr with
  | Ok v -> Ok (Obj.magic v)
  | Error err ->
      Brr.Console.error [ err ];
      Error (`Msg "Failed to unmarshal data")

let of_jv jv = Jv.to_jstr jv |> of_jstr

open! Std
module B64 = Base64
open! Brr
module Storage = Brr_io.Storage

let local_storage = Storage.local G.window

let with_var (type a) ~key (default : a) f =
  let k = Jstr.v key in
  let encode t = Marshal.to_string t [] |> B64.encode_string |> Jstr.v in
  let decode jstr : a =
    try
      let str = Jstr.to_string jstr |> B64.decode_exn in
      Marshal.from_string str 0
    with _ -> default
  in
  let value : a =
    (* unsafe ! add exception recovery*)
    match Storage.get_item local_storage k with
    | None -> default
    | Some v ->
        Console.log [ "unmarshaling:"; v ];
        decode v
  in
  let v = Lwd.var value in
  let x = Lwd.map (Lwd.get v) ~f:(fun v -> fun c -> Storage.set_item local_storage k (encode v) |> Result.get_exn; c) in
  Lwd.app x (f v)

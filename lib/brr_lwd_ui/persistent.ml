open! Std
module B64 = Base64
open! Brr
module Storage = Brr_io.Storage

let keys = Hashtbl.create 64

let check_key key =
  if Hashtbl.mem keys key then
    failwith @@ Format.sprintf "Multiple persitent var are using the key %s" key
  else Hashtbl.add keys key ()

let local_storage = Storage.local G.window
let encode t = Marshal.to_string t [] |> B64.encode_string |> Jstr.v

let decode ~key ~default jstr =
  try
    let str = Jstr.to_string jstr |> B64.decode_exn in
    Marshal.from_string str 0
  with _ ->
    Console.warn [ "Failed to decode data from local_storage:"; key; jstr ];
    default

let var ~key default =
  let () = check_key key in
  let key = Jstr.v key in
  let value =
    match Storage.get_item local_storage key with
    | None -> default
    | Some v -> decode ~key ~default v
  in
  let var = Lwd.var value in
  let root = Lwd.observe (Lwd.get var) in
  Lwd.set_on_invalidate root (fun _ ->
      let new_value = encode @@ Lwd.quick_sample root in
      ignore @@ Storage.set_item local_storage key new_value);
  let _ = Lwd.quick_sample root in
  var

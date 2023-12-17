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

let decode jstr =
  let open Result.Infix in
  try
    let+ str = Jstr.to_string jstr |> B64.decode in
    Marshal.from_string str 0
  with _ -> Error (`Msg "Failed to unmarshal data from local_storage:")

let var_f ~key f =
  Console.log [ "new var"; key ];
  let () = check_key key in
  let key = Jstr.v key in
  let value =
    match Storage.get_item local_storage key with
    | None -> f ()
    | Some v -> (
        match decode v with
        | Ok v -> v
        | Error (`Msg msg) ->
            Console.log [ msg ];
            f ())
  in
  let var = Lwd.var value in
  let root = Lwd.observe (Lwd.get var) in
  Lwd.set_on_invalidate root (fun _ ->
      let new_value = encode @@ Lwd.quick_sample root in
      ignore @@ Storage.set_item local_storage key new_value);
  let _ = Lwd.quick_sample root in
  var

let var ~key default = var_f ~key (fun () -> default)

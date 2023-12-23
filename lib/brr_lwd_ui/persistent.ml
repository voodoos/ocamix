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

let store ~key value = encode value |> Storage.set_item local_storage key

let fetch ~key =
  let open Result.Infix in
  let* encoded_value =
    match Storage.get_item local_storage key with
    | None -> Error `Not_found
    | Some v -> Ok v
  in
  decode encoded_value

let initial_value ~key f =
  match fetch ~key with
  | Error `Not_found -> f ()
  | Error (`Msg msg) ->
      Console.warn [ msg ];
      f ()
  | Ok v -> v

let initial_value_fut ~key f =
  match fetch ~key with
  | Error `Not_found -> f ()
  | Error (`Msg msg) ->
      Console.warn [ msg ];
      f ()
  | Ok v -> Fut.ok @@ v

let make_persistent_var ~key value =
  ignore @@ store ~key value;
  let var = Lwd.var value in
  let root = Lwd.observe (Lwd.get var) in
  Lwd.set_on_invalidate root (fun _ ->
      let new_value = Lwd.quick_sample root in
      ignore @@ store ~key new_value);
  let _ = Lwd.quick_sample root in
  var

let var_f ~key f =
  let () = check_key key in
  let key = Jstr.v key in
  let value = initial_value ~key f in
  make_persistent_var ~key value

let var ~key default = var_f ~key (fun () -> default)

let var_fut (type a b) ~key (f : unit -> (a, b) Fut.result) :
    (a Lwd.var, b) Fut.result =
  let () = check_key key in
  let key = Jstr.v key in
  let open Fut.Result_syntax in
  let+ (value : a) = (initial_value_fut ~key f : (a, b) Fut.result) in
  make_persistent_var ~key value

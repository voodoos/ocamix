open Import
module B64 = Base64
open! Brr
module Storage = Brr_io.Storage

let local_storage = Storage.local G.window

let store ~key value =
  Encodings.to_jstr value |> Storage.set_item local_storage key

let fetch ~key =
  let open Result.Infix in
  let* encoded_value =
    match Storage.get_item local_storage key with
    | None -> Error `Not_found
    | Some v -> Ok v
  in
  Encodings.of_jstr encoded_value

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
  let key = Jstr.v key in
  let value = initial_value ~key f in
  make_persistent_var ~key value

let var ~key default = var_f ~key (fun () -> default)

let var_fut (type a b) ~key (f : unit -> (a, b) Fut.result) :
    (a Lwd.var, b) Fut.result =
  let key = Jstr.v key in
  let open Fut.Result_syntax in
  let+ (value : a) = (initial_value_fut ~key f : (a, b) Fut.result) in
  make_persistent_var ~key value

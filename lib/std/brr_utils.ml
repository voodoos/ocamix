open Brr

let with_timing name f =
  let open Brr.Performance in
  let before = now_ms G.performance in
  let result = f () in
  Console.log [ name; " took"; now_ms G.performance -. before; "ms" ];
  result

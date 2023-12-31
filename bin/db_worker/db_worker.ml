open Brr
open Lib.Db_worker_api

module Worker () = struct
  let on_start () =
    Console.log [ "TOTO worker"; Brr_webworkers.Worker.ami () |> Jv.of_bool ]

  let on_query (type a) (q : a query) : a = match q with Get_all () -> []
  let () = on_start ()
end

include Make_worker (Worker)

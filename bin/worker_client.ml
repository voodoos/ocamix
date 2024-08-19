open Import
open Brr

include Db.Worker_api.Start_client (struct
  let url = "./db_worker.bc.js"
end)

let servers_status =
  let var = Lwd.var ("", Db.Sync.initial_report) in
  let _ =
    listen Servers_status_update ~f:(fun (id, report) ->
        Console.log
          [ Format.asprintf "Server %s: %a" id Db.Sync.pp_report report ];
        Lwd.set var (id, report))
  in
  var

let create_view v =
  let var = Lwd.var None in
  Fut.await (query (Create_view v)) (fun v -> Lwd.set var (Some v));
  Lwd.get var

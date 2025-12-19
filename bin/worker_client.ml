open Import
open Brr

include Db.Worker_api.Start_client (struct
  let url = "./db_worker.bc.js"
end)

let servers_status =
  let var = Lwd.var ("", Db.Sync.initial_report) in
  let _ =
    listen Servers_status_update ~f:(fun (id, report) ->
        Lwd.set var (id, report))
  in
  var

let update ?(eq = Equal.poly) var next =
  let current = Lwd.peek var in
  if not (eq current next) then Lwd.set var next

let get_view_item_count =
  let memo : (View.req, int Lwd.var) Hashtbl.t =
    (* todo memory leak *) Hashtbl.create 64
  in
  fun v ->
    let item_count = Hashtbl.get_or_add memo ~k:v ~f:(fun _ -> Lwd.var 0) in
    Fut.await (query Create_view v) (function
      | Ok v ->
          Console.debug
            [
              "Item count:";
              v.View.item_count;
              "Start offset:";
              v.View.start_offset;
            ];
          update item_count v.item_count
      | error ->
          Console.debug [ "ERROR"; error ];
          update item_count 0);
    Lwd.get item_count

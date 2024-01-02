open! Std
open Lib.Db_worker_api
open Brrer
module IDB = Brr_io.Indexed_db

module Worker () = struct
  let source =
    let source, set_source = Fut.create () in
    let _ =
      let open Data_source.Jellyfin in
      let base_url = "http://localhost:8096" in
      let username = "root" in
      let pw = "rootlocalroot" in
      let open Fut.Syntax in
      let+ source =
        connect ~base_url Api.Authenticate_by_name.{ username; pw }
      in
      set_source source
    in
    source

  let check_db idb =
    let open Fut.Result_syntax in
    let* source = source in
    Db.Sync.check_and_sync ~source idb

  let idb =
    let idb, set_idb = Fut.create () in
    let _ =
      let open Fut.Result_syntax in
      Db.with_idb ~name:"tracks" ~version:1 @@ fun idb ->
      ignore
        (let+ () = check_db idb in
         set_idb @@ Ok idb)
    in
    idb

  let on_start () =
    Brr.Console.log [ "pouet0" ];
    Brr.Console.log
      [ "TOTO worker"; Brr_webworkers.Worker.ami () |> Jv.of_bool ]

  let on_query (type a) (q : a query) : (a, [ `Msg of string ]) Fut.result =
    match q with
    | Get_all () ->
        let open Fut.Result_syntax in
        let* idb = idb in
        let store =
          IDB.Database.transaction [ (module Db.I) ] ~mode:Readonly idb
          |> IDB.Transaction.object_store (module Db.I)
        in
        let result, set_result = Fut.create () in
        let f = Brr.Performance.now_ms Brr.G.performance in
        let req = Db.I.get_all store in
        let _ =
          IDB.Request.on_success req ~f:(fun _ req ->
              let result = IDB.Request.result req in
              Brr.Console.log
                [ "took"; Brr.Performance.now_ms Brr.G.performance -. f; "ms" ];
              set_result (Ok (Array.to_list result)))
        in
        result

  let () = on_start ()
end

include Make_worker (Worker)

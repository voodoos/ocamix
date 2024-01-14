open Import
open Brr

type server = { connexion : DS.connexion; status : Db.Sync.report Lwd.var }
type t = (string * server) Lwd_seq.t Lwd.var

let var : t = Lwd.var Lwd_seq.empty

let connect ~base_url ~username ~password =
  let open Fut.Result_syntax in
  let+ connexion = DS.connect { base_url; username; password } in
  let status = Lwd.var Db.Sync.initial_report in
  let server = { connexion; status } in
  let server_id = connexion.auth_response.server_id in
  let _ =
    Worker_client.listen Servers_status_update ~f:(fun (id, report) ->
        (* TODO: subscribe to a specific server's updates *)
        if String.equal server_id id then Lwd.set status report)
  in
  let _ = Worker_client.query @@ Add_servers [ (server_id, connexion) ] in
  Lwd.update
    (fun servers -> Lwd_seq.(concat servers (element (server_id, server))))
    var

module Connect_form = struct
  open Brr_lwd_ui.Form

  type t = {
    url : string Field.validation;
    username : string Field.validation;
    password : string Field.validation;
  }

  let default = { url = Empty; username = Empty; password = Empty }

  let fields =
    let username_field =
      Field.text_input ~id:"username" { placeholder = "again" } ()
      |> Lwd.map ~f:(fun username_input ->
             F (username_input, fun t v -> { t with username = v }))
    in
    (* TODO: this should be simpler *)
    Lwd.return (Lwd_seq.element username_field)
end

let ui_form () =
  let open Brr_lwd_ui.Form in
  create (module Connect_form) (fun t -> Console.log [ "Form submitted:"; t ])

let ui_status server =
  let status =
    Lwd.map (Lwd.get server.status) ~f:(fun { status; sync_progress } ->
        match (status, sync_progress) with
        | In_sync, None -> El.txt' "Synchronized"
        | _, Some { Db.Sync.total; remaining } ->
            El.txt'
            @@ Printf.sprintf "Sync in progress: %i/%i" (total - remaining)
                 total
        | _ -> El.txt' "Desynchronized")
  in
  status

let ui () =
  let servers = Lwd.get var in
  let statuses = Lwd_seq.map (fun (_, server) -> ui_status server) servers in
  Elwd.div [ `R (ui_form ()); `S (Lwd_seq.lift statuses) ]

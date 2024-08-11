open Import
open Brr

type server = {
  connexion : DS.connexion;
  status : Db.Sync.report Lwd.var;
  refresh : unit Lwd.var;
}

let connexions : (string * DS.connexion) Lwd_seq.t Lwd.var =
  Brr_lwd_ui.Persistent.var ~key:"ui_servers" Lwd_seq.empty

let connect (server_id, { connexion; status; refresh }) =
  let _ =
    Worker_client.listen Servers_status_update ~f:(fun (id, report) ->
        (* TODO: subscribe to a specific server's updates *)
        let previous_status = Lwd.peek status in
        if String.equal server_id id then (
          Lwd.set status report;
          match (previous_status.sync_progress, report.sync_progress) with
          | Some { remaining; _ }, Some { remaining = remaining'; _ }
            when remaining <> remaining' ->
              Lwd.set refresh ()
          | Some { remaining; _ }, None -> Lwd.set refresh ()
          | _ -> ()))
  in
  ignore (Worker_client.query @@ Add_servers [ (server_id, connexion) ])

let servers_with_status =
  Lwd_seq.map
    (fun (id, connexion) ->
      let status = Lwd.var Db.Sync.initial_report in
      let refresh = Lwd.var () in
      let server = (id, { connexion; status; refresh }) in
      connect server;
      server)
    (Lwd.get connexions)

let new_connexion ~base_url ~username ~password =
  let open Fut.Result_syntax in
  let+ connexion = DS.connect { base_url; username; password } in
  let server_id = connexion.auth_response.server_id in
  (* TODO CHECK SERVER ID *)
  Lwd.update
    (fun servers -> Lwd_seq.(concat servers (element (server_id, connexion))))
    connexions

module Connect_form = struct
  open Brr_lwd_ui.Form

  type t = {
    url : string Field.validation;
    username : string Field.validation;
    password : string Field.validation;
  }

  let default = { url = Empty; username = Empty; password = Empty }

  let fields =
    let url_field =
      field
        (Lwd.pure
        @@ Field.text_input ~required:true (Some "https://demo.jellyfin.org/stable"))
        (fun t v -> { t with url = v })
    in
    let username_field =
      field
        (Lwd.pure @@ Field.text_input ~required:true (Some "demo"))
        (fun t v -> { t with username = v })
    in
    let password_field =
      field
        (Lwd.pure @@ Field.password_input ~required:false None)
        (fun t v -> { t with password = v })
    in
    let submit =
      field (Lwd.pure @@ Field.submit (`P "Connect")) (fun t _v -> t)
    in
    Lwd.return
      (Lwd_seq.of_list [ url_field; username_field; password_field; submit ])
end

let ui_form () =
  let open Brr_lwd_ui.Form in
  create
    (module Connect_form)
    (fun t ->
      Console.log [ "Form submitted:"; t ];
      match t with
      (* FIXME: validation already happened, it's redundant to have to match *)
      | { url = Ok url; username = Ok username; password = Ok password } ->
          Console.log [ "Form submitted:"; url; username ];
          ignore @@ new_connexion ~base_url:url ~username ~password
      | { url = Ok url; username = Ok username; _ } ->
          Console.log [ "Form submitted:"; url; username ];
          ignore @@ new_connexion ~base_url:url ~username ~password:""
      | _ -> ())

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

let fut_to_lwd ~init f =
  let v = Lwd.var init in
  let () = Fut.await f (Lwd.set v) in
  Lwd.get v

let servers_libraries =
  Lwd_seq.map
    (fun (server_id, { refresh; _ }) ->
      let views =
        Lwd.bind (Lwd.get refresh) ~f:(fun () ->
            Worker_client.query (Get_server_libraries server_id)
            |> Fut.map (Result.get_or ~default:[])
            |> fut_to_lwd ~init:[])
      in
      (server_id, views))
    servers_with_status

let ui () =
  let statuses =
    Lwd_seq.map (fun (_, server) -> ui_status server) servers_with_status
  in
  let ui_form =
    Lwd.map servers_with_status ~f:(fun s ->
        match Lwd_seq.view s with
        | Empty -> Lwd_seq.element @@ Elwd.div [ `R (ui_form ()) ]
        | _ -> Lwd_seq.empty)
  in
  Elwd.div [ `S (Lwd_seq.lift ui_form); `S (Lwd_seq.lift statuses) ]

open Import
open Brr

type server = { connexion : DS.connexion; status : Db.Sync.report Lwd.var }
type t = (string * server) Lwd_seq.t Lwd.var

let var : t = Brr_lwd_ui.Persistent.var ~key:"ui_servers" Lwd_seq.empty

let connect (server_id, { connexion; status }) =
  let _ =
    Worker_client.listen Servers_status_update ~f:(fun (id, report) ->
        (* TODO: subscribe to a specific server's updates *)
        if String.equal server_id id then Lwd.set status report)
  in
  ignore (Worker_client.query @@ Add_servers [ (server_id, connexion) ])

let () =
  (* Connect to servers that are already known when loading the page *)
  let servers = Lwd.peek var |> Lwd_seq.to_list in
  List.iter servers ~f:connect

let new_connexion ~base_url ~username ~password =
  let open Fut.Result_syntax in
  let+ connexion = DS.connect { base_url; username; password } in
  let status = Lwd.var Db.Sync.initial_report in
  let server = { connexion; status } in
  let server_id = connexion.auth_response.server_id in
  (* TODO CHECK SERVER ID *)
  let () = connect (server_id, server) in
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
    let url_field =
      field
        (Field.text_input
           ~at:[ `P (At.value (Jstr.v "http://localhost:8096")) ]
           ~required:true "")
        (fun t v -> { t with url = v })
    in
    let username_field =
      field (Field.text_input ~required:true "") (fun t v ->
          { t with username = v })
    in
    let password_field =
      field (Field.password_input ~required:true "") (fun t v ->
          { t with password = v })
    in
    let submit = field (Field.submit (`P "Connect")) (fun t _v -> t) in
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
  let statuses =
    Lwd_seq.map
      (fun (server_id, { status; _ }) ->
        let views =
          Lwd.bind (Lwd.get status) ~f:(fun _ ->
              Worker_client.query (Get_server_libraries server_id)
              |> Fut.map (Result.get_or ~default:[])
              |> fut_to_lwd ~init:[])
        in
        (server_id, views))
      (Lwd.get var)
  in
  statuses

let ui () =
  let servers = Lwd.get var in
  let statuses = Lwd_seq.map (fun (_, server) -> ui_status server) servers in
  let ui_form =
    Lwd.map servers ~f:(fun s ->
        match Lwd_seq.view s with
        | Empty -> Lwd_seq.element @@ Elwd.div [ `R (ui_form ()) ]
        | _ -> Lwd_seq.empty)
  in
  Elwd.div [ `S (Lwd_seq.lift ui_form); `S (Lwd_seq.lift statuses) ]

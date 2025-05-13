open Brrer.Brr
open Brr_lwd
open Brr_io

type status = Ready | Loading | Error | Ok

let make ~at ?(placeholder : string option) ?(hold = Fut.return ())
    (url_source : string option Lwd.t) =
  let status = Lwd.var Ready in
  let object_url = Lwd.var placeholder in
  let () =
    let open Fut.Syntax in
    Utils.listen url_source ~f:(fun url ->
        Fut.await
          (let* () = hold in
           Lwd.set status Loading;
           match url with
           | None ->
               Lwd.set status Error;
               Fut.ok @@ Lwd.set object_url None
           | Some url -> (
               let* result = Fetch.url (Jstr.v url) in
               match result with
               | Ok response when Fetch.Response.ok response ->
                   let open Fut.Result_syntax in
                   let+ blob = Fetch.(Response.as_body response |> Body.blob) in
                   Console.log [ ("LOADED", url) ];
                   Lwd.set status Ok;
                   Lwd.set object_url (Some (Url.create_object_url blob))
               | _ ->
                   Console.log [ ("ERROR", url) ];
                   Lwd.set status Error;
                   Fut.ok @@ Lwd.set object_url None))
          ignore)
  in
  let on_load =
    Lwd.map (Lwd.get object_url) ~f:(fun src ->
        (* Revoking is necessary to free memory *)
        (* TODO However we might still leak some object urls if the url_source
        changes before the on_load event is fired. A LRU cache might be a way to
        get around that. Another option could be to use the status var. *)
        Elwd.handler Ev.load (fun _ -> Option.iter Url.revoke_object_url src))
  in
  let src =
    Lwd.map (Lwd.get object_url) ~f:(function
      | None -> At.void
      | Some src -> At.src (Jstr.v src))
  in
  (Elwd.img ~ev:[ `R on_load ] ~at:(`R src :: at) (), status)

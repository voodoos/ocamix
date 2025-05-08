open Brrer.Brr
open Brr_lwd
open Brr_io

type status = Loading | Error | Ok

let make ~at ~(placeholder : string) (url : string) =
  let status = Lwd.var Loading in
  let src = Lwd.var placeholder in
  let _ =
    let open Fut.Syntax in
    let* result = Fetch.url (Jstr.v url) in
    match result with
    | Error _ ->
        Lwd.set status Error;
        Fut.ok ()
    | Ok response ->
        let open Fut.Result_syntax in
        let+ blob = Fetch.(Response.as_body response |> Body.blob) in
        Lwd.set status Ok;
        Lwd.set src @@ Url.create_object_url blob
  in
  let on_load =
    Lwd.map (Lwd.get src) ~f:(fun src ->
        (* Revoking is necessary to free memory *)
        Elwd.handler Ev.load (fun _ -> Url.revoke_object_url src))
  in
  let src = Lwd.map (Lwd.get src) ~f:(fun src -> At.src (Jstr.v src)) in
  (Elwd.img ~ev:[ `R on_load ] ~at:(`R src :: at) (), status)

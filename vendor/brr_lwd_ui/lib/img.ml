open Brrer.Brr
open Brr_lwd
open Brr_io

type status = Ready | Loading | Error | Ok
type preloader = { object_url : string option Lwd.var; status : status Lwd.var }

let pp_status fmt status =
  Format.fprintf fmt "%s"
    (match status with
    | Ready -> "ready"
    | Loading -> "loading"
    | Error -> "error"
    | Ok -> "ok")

let preloader ?(placeholder : string option)
    (url_source : string option Fut.t Lwd.t) =
  let status = Lwd.var Ready in
  let object_url = Lwd.var placeholder in
  let () =
    let open Fut.Syntax in
    let previous_url = ref "" in
    let previous_fetch_abort : (unit -> unit) ref = ref Fun.id in
    Utils.listen url_source ~f:(fun url ->
        Fut.await
          (* TODO we should not wait but cancel/ignore previous requests.
             See https://developer.mozilla.org/en-US/docs/Web/API/AbortController/abort *)
          (let* url = url in
           match url with
           | None ->
               Lwd.set status Ready;
               Fut.ok ()
           | Some url when String.equal !previous_url url ->
               Lwd.set status Ok;
               Fut.ok ()
           | Some url -> (
               Lwd.set status Loading;
               let abort_controller = Abort.controller () in
               let () =
                 !previous_fetch_abort ();
                 previous_fetch_abort := fun () -> Abort.abort abort_controller
               in
               let* result =
                 let signal = Abort.signal abort_controller in
                 let init = Fetch.Request.init ~signal () in
                 Fetch.url ~init (Jstr.v url)
               in
               match result with
               | Ok response when Fetch.Response.ok response ->
                   let open Fut.Result_syntax in
                   let+ blob = Fetch.(Response.as_body response |> Body.blob) in
                   previous_url := url;
                   (* Revoking is necessary to prevent memory leaks. It might be
                      a good idea to use some kind of resource LRU to cache more
                      results.  *)
                   (* TODO we never release the last image when the element is
                      removed from the dom, we should. *)
                   Option.iter Url.revoke_object_url (Lwd.peek object_url);
                   Lwd.set object_url (Some (Url.create_object_url blob));
                   Lwd.set status Ok
               | Ok _ ->
                   (* This happens when the http query errored *)
                   Lwd.set status Error;
                   Fut.ok ()
               | Error _ ->
                   (* This happens when the future is aborted. We could check
                      the exception name for more precise reporting. *)
                   Fut.ok ()))
          ignore)
  in
  { object_url; status }

let object_url t = Lwd.get t.object_url
let status t = Lwd.get t.status

let loading_at ?(name = "loading") t =
  Lwd.map (status t) ~f:(fun status ->
      match status with Loading -> At.class' (Jstr.v name) | _ -> At.void)

let render_img ?(revoke_on_load = false) ~at ({ object_url; _ } as t) =
  let ev =
    if not revoke_on_load then []
    else
      let on_load =
        Lwd.map (Lwd.get object_url) ~f:(fun src ->
            Elwd.handler Ev.load (fun _ ->
                Option.iter Url.revoke_object_url src))
      in
      [ `R on_load ]
  in
  let src =
    Lwd.map (Lwd.get object_url) ~f:(function
      | None -> At.void
      | Some src -> At.src (Jstr.v src))
  in
  let loading = loading_at t in
  let s_at = Lwd.return @@ Lwd_seq.of_list [ src; loading ] in
  Elwd.img ~ev ~at:(`S (Lwd_seq.lift s_at) :: at) ()

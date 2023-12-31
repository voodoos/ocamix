open Std
open Brr

module type Queries = sig
  type 'a query
end

type 'a with_uuid = { uuid : string; message : 'a }

module Make (Q : Queries) = struct
  type 'a query = 'a Q.query
  (* todo:check that the worker and the client share the same api? *)

  let futures : (string, Jv.t -> unit) Hashtbl.t = Hashtbl.create 64
  let random_state = Random.get_state ()

  module Client (P : sig
    val url : string
  end) =
  struct
    let worker = Brr_webworkers.Worker.create @@ Jstr.of_string P.url

    let query (type a) (query : a query) : (a, [ `Msg of string ]) Fut.result =
      let uuid = Uuidm.(v4_gen random_state () |> to_string) in
      let fut, set = Fut.create () in
      let set jv = set @@ Encodings.unmarshal_jv jv in
      let query = { uuid; message = query } |> Encodings.marshal_to_jstr in
      Brr_webworkers.Worker.post worker (Jv.of_jstr query);
      Hashtbl.add futures uuid set;
      fut

    let on_message ev =
      let message = Ev.as_type ev in
      let open Result in
      ignore (* TODO: handler errors *)
      @@
      let+ ({ uuid; message } : Jv.t with_uuid) =
        Brr_io.Message.Ev.data message |> Jv.to_jstr |> Encodings.unmarshal_jstr
      in
      let set = Hashtbl.find futures uuid in
      Hashtbl.remove futures uuid;
      set message

    let _ =
      Ev.listen Brr_io.Message.Ev.message on_message
      @@ Brr_webworkers.Worker.as_target worker
  end

  module type Worker = functor () -> sig
    val on_query : 'a query -> 'a
  end

  (** Execute W's body and configure messaging *)
  module Make_worker (W : Worker) = struct
    open Brr
    module W = W ()

    let on_message ev =
      let message = Ev.as_type ev in
      let open Result in
      ignore (* TODO: handler errors *)
      @@
      let+ ({ uuid; message } : 'a query with_uuid) =
        Brr_io.Message.Ev.data message |> Jv.to_jstr |> Encodings.unmarshal_jstr
      in
      let result = W.on_query message in
      let message = Encodings.marshal_to_jstr result |> Jv.of_jstr in
      Brr_webworkers.Worker.G.post
        (Encodings.marshal_to_jstr { uuid; message } |> Jv.of_jstr)

    let _ = Ev.listen Brr_io.Message.Ev.message on_message G.target
  end
end

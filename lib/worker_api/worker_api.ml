open Std
open Brr

type tag = Block of int | Int of int

let tag_of v =
  (* wow wow wow. is that okayyish ?*)
  let obj = Obj.repr v in
  if Obj.is_block obj then Block (Obj.tag obj) else Int (Obj.magic obj)

module type Queries = sig
  type 'a query
  type 'a event
end

module Make (Q : Queries) = struct
  type error = [ `Jv of Jv.Error.t | `Msg of string ]
  type 'a query = 'a Q.query
  type 'a event = 'a Q.event
  type 'a with_uuid = { uuid : string; data : 'a }
  type 'a message = Answer of 'a with_uuid | Event of ('a event * 'a)
  type listener = string

  (* todo:check that the worker and the client share the same api? *)

  module Start_client (P : sig
    val url : string
  end) =
  struct
    let futures : (string, Jv.t -> unit) Hashtbl.t = Hashtbl.create 64
    let listeners : (tag, Jv.t -> unit) Hashtbl.t = Hashtbl.create 64
    let worker = Brr_webworkers.Worker.create @@ Jstr.of_string P.url

    let query (type a) (query : a query) : (a, error) Fut.result =
      let uuid = new_uuid_v4 () |> Uuidm.to_string in
      let fut, set = Fut.create () in
      let set jv = set @@ Encodings.of_jv jv in
      let query = { uuid; data = query } |> Encodings.to_jstr in
      Brr_webworkers.Worker.post worker (Jv.of_jstr query);
      Hashtbl.add futures uuid set;
      fut

    let listen (type a) (event : a event) ~(f : a -> unit) : listener =
      let uuid = new_uuid_v4 () |> Uuidm.to_string in
      let set jv = f (Obj.magic jv) in
      Hashtbl.add listeners (tag_of event) set;
      uuid

    let on_message ev =
      let message = Ev.as_type ev in
      let open Result in
      ignore (* TODO: handler errors *)
      @@
      let+ message =
        Brr_io.Message.Ev.data message |> Jv.to_jstr |> Encodings.of_jstr
      in
      match message with
      | Event (e, v) ->
          Hashtbl.find_all listeners (tag_of e) |> List.iter ~f:(fun f -> f v)
      | Answer { uuid; data } ->
          let f = Hashtbl.find futures uuid in
          Hashtbl.remove futures uuid;
          f data

    let _ =
      Ev.listen Brr_io.Message.Ev.message on_message
      @@ Brr_webworkers.Worker.as_target worker
  end

  module type Worker_impl = functor () -> sig
    val on_query : 'a query -> ('a, error) Fut.result
  end

  let dispatch_event (type a) (e : a event) (v : a) =
    (* let data = Encodings.marshal_to_jstr |> Jv.of_jstr in *)
    Brr_webworkers.Worker.G.post (Encodings.to_jstr (Event (e, v)) |> Jv.of_jstr)

  (** Execute W's body and configure messaging *)
  module Make_worker (W : Worker_impl) = struct
    open Brr
    module W = W ()

    let on_message ev =
      let message = Ev.as_type ev in
      let open Result in
      ignore (* TODO: handler errors *)
      @@
      let+ ({ uuid; data } : 'a query with_uuid) =
        Brr_io.Message.Ev.data message |> Jv.to_jstr |> Encodings.of_jstr
      in
      let open Fut.Result_syntax in
      let+ result = W.on_query data in
      let data = Encodings.to_jstr result |> Jv.of_jstr in
      Brr_webworkers.Worker.G.post
        (Encodings.to_jstr (Answer { uuid; data }) |> Jv.of_jstr)

    let _ = Ev.listen Brr_io.Message.Ev.message on_message G.target
  end
end

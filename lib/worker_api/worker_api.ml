open Brr
module List = Stdlib.List

type tag = Block of int | Int of int [@@deriving jsont]

open Std

let tag_of v =
  (* wow wow wow. is that okayyish ?*)
  let obj = Obj.repr v in
  if Obj.is_block obj then Block (Obj.tag obj) else Int (Obj.magic obj)

type 'a transfer = {
  encode : 'a -> (Jv.t, [ `Jv of Jv.Error.t | `Msg of string ]) result;
  decode : Jv.t -> ('a, [ `Jv of Jv.Error.t | `Msg of string ]) result;
  transferables : (Jv.t -> Jv.t) list;
}

type 'a transfer_or_conv = Transfer of 'a transfer | Conv of 'a Jsont.t

module type Queries = sig
  type ('a, 'b) query
  type 'a event

  val jsont : ('a, 'b) query -> 'a Jsont.t * 'b transfer_or_conv
  val event_jsont : 'a event -> 'a Jsont.t
end

module Make (Q : Queries) = struct
  type error = [ `Jv of Jv.Error.t | `Msg of string ]
  type ('a, 'b) query = ('a, 'b) Q.query
  type 'a event = 'a Q.event
  type 'a with_uuid = { uuid : Jstr.t; data : 'a }
  type 'a message = Answer of Jv.t with_uuid | Event of (tag * Jv.t)
  type listener = string

  (* todo:check that the worker and the client share the same api? *)

  let decode_jv jsont jv =
    Jsont_brr.decode_jv jsont jv |> Result.map_err (fun e -> `Jv e)

  let j_kind = Jstr.v "k"
  let j_kind_a = Jv.of_string "a"
  let j_kind_answer = (j_kind, j_kind_a)
  let j_kind_event = (j_kind, Jv.of_string "e")
  let j_uuid = Jstr.v "u"
  let j_query = Jstr.v "q"
  let j_data = Jstr.v "d"
  let j_tag = Jstr.v "t"

  let encode_message = function
    | Answer { uuid; data } ->
        Jv.obj' [| j_kind_answer; (j_uuid, Jv.of_jstr uuid); (j_data, data) |]
    | Event (tag, data) ->
        let tag = Jsont_brr.encode_jv' tag_jsont tag |> Result.get_exn in
        Jv.obj' [| j_kind_event; (j_tag, tag); (j_data, data) |]

  let decode_message obj =
    match Jv.get' obj j_kind with
    | k when Jv.equal j_kind_a k ->
        let uuid = Jv.get' obj j_uuid |> Jv.to_jstr in
        let data = Jv.get' obj j_data in
        Answer { uuid; data }
    | _ ->
        let tag =
          Jv.get' obj j_tag |> Jsont_brr.decode_jv' tag_jsont |> Result.get_exn
        in
        let data = Jv.get' obj j_data in
        Event (tag, data)

  module Start_client (P : sig
    val url : string
  end) =
  struct
    let futures : (Jstr.t, Jv.t -> unit) Hashtbl.t = Hashtbl.create 64
    let listeners : (tag, Jv.t -> unit) Hashtbl.t = Hashtbl.create 64
    let worker = Brr_webworkers.Worker.create @@ Jstr.of_string P.url

    let query (type a b) (query : (a, b) query) (data : a) :
        (b, error) Fut.result =
      let uuid = new_uuid_v4 () |> Uuidm.to_string |> Jstr.of_string in
      let fut, set = Fut.create () in
      let encoder, decoder = Q.jsont query in
      let set jv =
        match decoder with
        | Conv d -> set (decode_jv d jv)
        | Transfer { decode; _ } -> set (decode jv)
      in
      let data = Jsont_brr.encode_jv encoder data |> Result.get_exn in
      let query = Encodings.to_jv query in
      let query =
        Jv.obj'
          [| (j_uuid, Jv.of_jstr uuid); (j_query, query); (j_data, data) |]
      in
      Console.debug [ "Worker posts query"; query ];
      Brr_webworkers.Worker.post worker query;
      Hashtbl.add futures uuid set;
      fut

    let listen (type a) (event : a event) ~(f : a -> unit) : listener =
      let uuid = new_uuid_v4 () |> Uuidm.to_string in
      let set jv =
        f (Jsont_brr.decode_jv' (Q.event_jsont event) jv |> Result.get_exn)
      in
      Hashtbl.add listeners (tag_of event) set;
      uuid

    let on_message ev =
      let message = Ev.as_type ev in
      let message = Brr_io.Message.Ev.data message |> decode_message in
      match message with
      | Event (tag, v) ->
          Hashtbl.find_all listeners tag |> List.iter ~f:(fun f -> f v)
      | Answer { uuid; data } ->
          let f = Hashtbl.find futures uuid in
          Hashtbl.remove futures uuid;
          f data

    let _ =
      Ev.listen Brr_io.Message.Ev.message on_message
      @@ Brr_webworkers.Worker.as_target worker
  end

  module type Worker_impl = functor () -> sig
    val on_query : ('a, 'b) query -> 'a -> ('b, error) Fut.result
  end

  let dispatch_event (type a) (e : a event) (v : a) =
    let v = Jsont_brr.encode_jv' (Q.event_jsont e) v |> Result.get_exn in
    let message = encode_message (Event (tag_of e, v)) in
    Console.debug [ "Worker posts event"; message ];
    Brr_webworkers.Worker.G.post message

  (** Execute W's body and configure messaging *)
  module Make_worker (W : Worker_impl) = struct
    open Brr
    module W = W ()

    let on_message ev =
      let message = Ev.as_type ev in
      let open Result in
      ignore (* TODO: handler errors *)
      @@
      let+ uuid, query, data, encoder =
        let obj = Brr_io.Message.Ev.data message in
        Console.debug [ "Worker received"; obj ];
        let uuid = Jv.get' obj j_uuid in
        let* query : _ query = Encodings.of_jv (Jv.get' obj j_query) in
        let decoder, encoder = Q.jsont query in
        let+ data = decode_jv decoder (Jv.get' obj j_data) in
        (uuid, query, data, encoder)
      in
      let open Fut.Result_syntax in
      let+ result = W.on_query query data in
      let uuid = Jv.to_jstr uuid in
      match encoder with
      | Conv encoder ->
          let data = Jsont_brr.encode_jv' encoder result |> Result.get_exn in
          let message = encode_message (Answer { uuid; data }) in
          Console.debug [ "Worker posts answer"; message ];
          Brr_webworkers.Worker.G.post message
      | Transfer { encode; transferables; _ } ->
          let data = encode result |> Result.get_exn in
          let message = encode_message (Answer { uuid; data }) in
          let transfer =
            List.map transferables ~f:(fun f ->
                Brr_io.Message.transfer @@ f (Jv.get' message j_data))
          in
          let opts = Brr_io.Message.opts ~transfer () in
          Console.debug [ "Worker posts answer"; message ];
          Brr_webworkers.Worker.G.post ~opts message

    let _ = Ev.listen Brr_io.Message.Ev.message on_message G.target
  end
end

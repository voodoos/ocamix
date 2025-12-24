open! Std
open Brr
open Worker_api

type hash_req = { hash : string; w : int; h : int } [@@deriving jsont]

module Queries = struct
  type ('a, 'b) query =
    | Render : (hash_req, (int, Bigarray.int8_unsigned_elt) Tarray.t) query

  let array_transfert =
    {
      encode = (fun tarray -> Result.return (Tarray.to_jv tarray));
      decode = (fun jv -> Result.return (Tarray.of_jv jv));
      transferables =
        [
          (fun tarray ->
            Tarray.of_jv tarray |> Tarray.buffer |> Tarray.Buffer.to_jv);
        ];
    }

  let jsont (type a b) (q : (a, b) query) : a Jsont.t * b transfer_or_conv =
    match q with Render -> (hash_req_jsont, Transfer array_transfert)

  type _ event

  let event_jsont (type a) (_ : a event) : a Jsont.t = assert false
end

include Worker_api.Make (Queries)

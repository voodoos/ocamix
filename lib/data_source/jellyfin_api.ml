open Brr

type method' = Get | Post

let jstr_of_method = function Get -> Jstr.v "GET" | Post -> Jstr.v "POST"

type user = {
  name : string; [@key "Name"]
  server_id : string; [@key "ServerId"]
  server_name : string option; [@default None] [@key "ServerName"]
  id : string; [@key "Id"]
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

module type Query = sig
  type params [@@deriving yojson]
  type response [@@deriving yojson]

  val method' : method'
  val endpoint : string
end

module Authenticate_by_name = struct
  type params = { username : string; [@key "Username"] pw : string [@key "Pw"] }
  [@@deriving yojson]

  type response = {
    user : user; [@key "User"]
    access_token : string; [@key "AccessToken"]
    server_id : string; [@key "ServerId"]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let method' = Post
  let endpoint = "/Users/AuthenticateByName"
end

module Item = struct
  type t = { name : string [@key "Name"] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Items = struct
  type params = { user_id : string [@key "userId"] } [@@deriving yojson]

  type response = {
    items : Item.t list; [@key "Items"]
    total_record_count : int; [@key "TotalRecordCount"]
    start_index : int; [@key "StartIndex"]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let method' = Get
  let endpoint = "/Items"
end

module System = struct
  module Info = struct
    type params = unit [@@deriving yojson]

    type response = {
      local_address : string option; [@default None] [@key "LocalAdress"]
      server_name : string; [@key "ServerName"]
      product_name : string option; [@default None] [@key "ProductName"]
      operating_system : string option; [@default None] [@key "OperatingSystem"]
      id : string; [@key "Id"]
    }
    [@@deriving yojson] [@@yojson.allow_extra_fields]

    let method' = Get
    let endpoint = "/System/Info"
  end
end

let authorization ?token () =
  Format.(
    let pp_token fmt = fprintf fmt ", Token=%S" in
    asprintf
      "MediaBrowser Client=\"Ocamix\", Device=\"Firefox\", DeviceId=\"0\", \
       Version=\"0.1\"%a"
      (pp_print_option pp_token) token)

let request (type p r) ?base_url ?token ?headers
    (module Q : Query with type params = p and type response = r) (params : p) :
    (r, Jv.Error.t) Fut.result =
  let open Brr_io.Fetch in
  let uri =
    Jstr.(Uri.of_jstr ?base:(Option.map v base_url) (v Q.endpoint))
    |> Result.get_ok
  in
  let authorization = authorization ?token () in
  let headers =
    Headers.of_assoc ?init:headers
      Jstr.
        [
          (v "content-type", v "text/json");
          (v "X-Emby-Authorization", v authorization);
        ]
  in
  let method' = jstr_of_method Q.method' in
  let init, url =
    match Q.method' with
    | Get ->
        let params =
          params |> Q.yojson_of_params |> Yojson.Safe.to_string |> Jstr.v
          |> Json.decode |> Result.get_ok |> Uri.Params.of_obj
        in
        let uri_with_params = Uri.with_query_params uri params in
        (Request.init ~headers ~method' (), Uri.to_jstr uri_with_params)
    | Post ->
        let body =
          params |> Q.yojson_of_params |> Yojson.Safe.to_string |> Jstr.v
          |> Body.of_jstr
        in
        (Request.init ~headers ~method' ~body (), Uri.to_jstr uri)
  in
  let open Fut.Result_syntax in
  let* res = request @@ Request.v ~init url in
  let+ json = Response.as_body res |> Body.text in
  Q.response_of_yojson (Yojson.Safe.from_string (Jstr.to_string json))
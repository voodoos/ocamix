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

let request (type p r) ?(base_url = "") ?token ?headers
    (module Q : Query with type params = p and type response = r) (params : p) :
    (r, Jv.Error.t) Fut.result =
  let open Brr_io.Fetch in
  let url = String.cat base_url Q.endpoint in
  let authorization = authorization ?token () in
  let headers =
    Headers.of_assoc ?init:headers
      Jstr.
        [
          (v "content-type", v "text/json");
          (v "X-Emby-Authorization", v authorization);
        ]
  in
  Console.log [ authorization ];
  let method' = jstr_of_method Q.method' in
  let init =
    match Q.method' with
    | Get -> Request.init ~headers ~method' ()
    | Post ->
        let body =
          params |> Q.yojson_of_params |> Yojson.Safe.to_string |> Jstr.v
          |> Body.of_jstr
        in
        Request.init ~headers ~method' ~body ()
  in
  let open Fut.Result_syntax in
  let* res = request @@ Request.v ~init (Jstr.v url) in
  let+ json = Response.as_body res |> Body.text in
  Q.response_of_yojson (Yojson.Safe.from_string (Jstr.to_string json))

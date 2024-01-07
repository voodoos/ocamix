module Api = Jellyfin_api

type credentials = { base_url : string; username : string; password : string }

type connexion = {
  base_url : string;
  auth_response : Api.Authenticate_by_name.response;
}

let get_token t = t.auth_response.Api.Authenticate_by_name.access_token

let connect credentials =
  let module Auth = Api.Authenticate_by_name in
  let open Fut.Result_syntax in
  let { base_url; username; password } = credentials in
  let auth = { Api.Authenticate_by_name.username; pw = password } in
  let+ auth_response = Api.request ~base_url (module Auth) auth in
  { base_url; auth_response }

let query t =
  let token = get_token t in
  let base_url = t.base_url in
  Jellyfin_api.request ~base_url ~token

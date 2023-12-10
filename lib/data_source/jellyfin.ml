module Api = Jellyfin_api

type connexion = {
  base_url : string;
  auth_response : Api.Authenticate_by_name.response;
}

type credentials = Api.Authenticate_by_name.params

let get_token t = t.auth_response.Api.Authenticate_by_name.access_token

let connect ~base_url credentials =
  let module Auth = Api.Authenticate_by_name in
  let open Fut.Result_syntax in
  let+ auth_response = Api.request ~base_url (module Auth) credentials in
  { base_url; auth_response }

let query t =
  let token = get_token t in
  let base_url = t.base_url in
  Jellyfin_api.request ~base_url ~token

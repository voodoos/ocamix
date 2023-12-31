open! Std
module Api = Data_source.Jellyfin.Api

module Queries = struct
  type 'a query = Get_all : unit -> Api.Item.t list query
end

include Worker_api.Make (Queries)

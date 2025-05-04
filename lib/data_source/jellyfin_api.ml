open Std
module J = Json
open Brr

type method' = Get | Post

let jstr_of_method = function Get -> Jstr.v "GET" | Post -> Jstr.v "POST"

module Types = struct
  type order = Ascending | Descending [@@deriving jsont]

  type sort =
    | Album
    | AlbumArtist
    | Artist
    | Budget
    | CommunityRating
    | CriticRating
    | DateCreated
    | DatePlayed
    | Default
    | IndexNumber
    | IsFolder
    | ParentIndexNumber
    | PlayCount
    | PremiereDate
    | ProductionYear
    | SortName
    | Random
    | Revenue
    | Runtime
  [@@deriving jsont]
end

type user = {
  name : string; [@key "Name"]
  server_id : string; [@key "ServerId"]
  server_name : string option; [@default None] [@key "ServerName"]
  id : string; [@key "Id"]
}
[@@deriving jsont]

module type Query = sig
  type path_params
  type params [@@deriving jsont]
  type response [@@deriving jsont]

  val method' : method'
  val endpoint : path_params -> string list
end

module Authenticate_by_name = struct
  type path_params = unit

  type params = { username : string; [@key "Username"] pw : string [@key "Pw"] }
  [@@deriving jsont]

  type response = {
    user : user; [@key "User"]
    access_token : string; [@key "AccessToken"]
    server_id : string; [@key "ServerId"]
  }
  [@@deriving jsont]

  let method' = Post
  let endpoint _ = [ "Users"; "AuthenticateByName" ]
end

module Item = struct
  type image_blur_hash = string String.StdMap.t

  let image_blur_hash_jsont = Jsont.Object.as_string_map Jsont.string

  type genre_item = { name : string; [@key "Name"] id : string [@key "Id"] }
  [@@deriving jsont]

  type artist_item = { name : string; [@key "Name"] id : string [@key "Id"] }
  [@@deriving jsont]

  type image_blur_hashes = {
    primary : image_blur_hash;
        [@default String.StdMap.empty]
        [@omit String.StdMap.is_empty]
        [@key "Primary"]
  }
  [@@deriving jsont]

  type type_ =
    | AggregateFolder
    | Audio
    | AudioBook
    | BasePluginFolder
    | Book
    | BoxSet
    | Channel
    | ChannelFolderItem
    | CollectionFolder
    | Episode
    | Folder
    | Genre
    | LiveTvChannel
    | LiveTvProgram
    | ManualPlaylistsFolder
    | Movie
    | MusicAlbum
    | MusicArtist
    | MusicGenre
    | MusicVideo
    | Person
    | Photo
    | PhotoAlbum
    | Playlist
    | PlaylistsFolder
    | Program
    | Recording
    | Season
    | Series
    | Studio
    | Trailer
    | TvChannel
    | TvProgram
    | UserRootFolder
    | UserView
    | Video
    | Year
  [@@deriving jsont]

  type field =
    | AirTime
    | CanDelete
    | CanDownload
    | ChannelImage
    | ChannelInfo
    | Chapters
    | ChildCount
    | CumulativeRunTimeTicks
    | CustomRating
    | DateCreated
    | DateLastMediaAdded
    | DateLastRefreshed
    | DateLastSaved
    | DisplayPreferencesId
    | EnableMediaSourceDisplay
    | Etag
    | ExternalEtag
    | ExternalSeriesId
    | ExternalUrls
    | ExtraIds
    | Genres
    | Height
    | HomePageUrl
    | InheritedParentalRatingValue
    | IsHD
    | ItemCounts
    | LocalTrailerCount
    | MediaSourceCount
    | MediaSources
    | MediaStreams
    | OriginalTitle
    | Overview
    | ParentId
    | Path
    | People
    | PlayAccess
    | PresentationUniqueKey
    | PrimaryImageAspectRatio
    | ProductionLocations
    | ProviderIds
    | RecursiveItemCount
    | RefreshState
    | RemoteTrailers
    | ScreenshotImageTags
    | SeasonUserData
    | SeriesPresentationUniqueKey
    | SeriesPrimaryImage
    | SeriesStudio
    | ServiceName
    | Settings
    | SortName
    | SpecialEpisodeNumbers
    | SpecialFeatureCount
    | Studios
    | SyncInfo
    | Taglines
    | Tags
    | ThemeSongIds
    | ThemeVideoIds
    | Width
  [@@deriving jsont]

  type external_url = { name : string; [@key "Name"] url : string [@key "Url"] }
  [@@deriving jsont]

  (* The [Type] field is actually a json string but we want to see it as a
     variant (which is a list of one string) *)
  type type_str = type_

  let type_str_jsont = type__jsont

  type t = {
    name : string; [@key "Name"]
    sort_name : string option; [@option] [@key "SortName"]
    external_urls : external_url list; [@default []] [@key "ExternalUrls"]
    id : string; [@key "Id"]
    path : string option; [@option] [@key "Path"]
    run_time_ticks : float; [@default 0.] [@key "RunTimeTicks"]
    is_folder : bool; [@default false] [@key "IsFolder"]
    album_id : string option; [@option] [@key "AlbumId"]
    parent_id : string option option;
        (* [ParentId] might absent, [null], or a string *)
        [@option]
        [@key "ParentId"]
    server_id : string; [@key "ServerId"]
    (* image_blur_hashes : image_blur_hashes; [@key "ImageBlurHashes"] *)
    type_ : type_str; [@key "Type"]
    genre_items : genre_item list; [@default []] [@key "GenreItems"]
    artist_items : artist_item list; [@default []] [@key "ArtistItems"]
    album_artists : artist_item list; [@default []] [@key "AlbumArtists"]
    collection_type : string option; [@option] [@key "CollectionType"]
  }
  [@@deriving jsont]
end

module Items = struct
  type path_params = unit

  type params = {
    ids : string list; [@default []] [@omit List.is_empty] [@key "ids"]
    parent_id : string option; [@option] [@key "parentId"]
    user_id : string; [@key "userId"]
    fields : Item.field list;
    include_item_types : Item.type_ list; [@key "includeItemTypes"]
    start_index : int option; [@option] [@key "startIndex"]
    limit : int option; [@option]
    sort_order : Types.order option; [@option] [@key "sortOrder"]
    sort_by : Types.sort list; [@key "sortBy"]
    recursive : bool;
    enable_user_data : bool; [@key "enableUserData"]
    enable_images : bool; [@key "enableImages"]
  }
  [@@deriving jsont]

  type response = {
    items : Item.t list; [@key "Items"]
    total_record_count : int; [@key "TotalRecordCount"]
    start_index : int; [@key "StartIndex"]
  }
  [@@deriving jsont]

  let method' = Get
  let endpoint _ = [ "Items" ]
end

module User_item = struct
  type path_params = { user_id : string; item_id : string }
  type params = unit [@@deriving jsont]
  type response = Item.t [@@deriving jsont]

  let method' = Get
  let endpoint pp = [ "Users"; pp.user_id; "Items"; pp.item_id ]
end

(* Only for priviledged users... *)
module Items_external_id_infos = struct
  type path_params = { item_id : string }
  type params = unit list [@@deriving yojson]

  type info = {
    name : string; [@key "Name"]
    key : string; [@key "Key"]
    type_ : string; [@key "Type"]
    url_format_string : string; [@key "UrlFormatString"]
  }
  [@@deriving jsont]

  type response = info list [@@deriving jsont]

  let method' = Get
  let endpoint { item_id } = [ "Items"; item_id; "ExternalIdInfos" ]
end

module Views = struct
  type path_params = { user_id : string }

  type params = {
    include_external_content : bool; [@key "includeExternalContent"]
  }
  [@@deriving jsont]

  type response = {
    items : Item.t list; [@key "Items"]
    total_record_count : int; [@key "TotalRecordCount"]
    start_index : int; [@key "StartIndex"]
  }
  [@@deriving jsont]

  let method' = Get
  let endpoint pp = [ "Users"; pp.user_id; "Views" ]
end

module Virtual_folders = struct
  type path_params = unit
  type params = unit [@@deriving jsont]

  type t = {
    name : string; [@key "Name"]
    locations : string list; [@key "Locations"]
    item_id : string; [@key "ItemId"]
  }
  [@@deriving jsont]

  type response = t list [@@deriving jsont]

  let method' = Get
  let endpoint _ = [ "Library"; "VirtualFolders" ]
end

module System = struct
  module Info = struct
    type path_params = unit
    type params = unit [@@deriving jsont]

    type response = {
      local_address : string option; [@default None] [@key "LocalAdress"]
      server_name : string; [@key "ServerName"]
      product_name : string option; [@default None] [@key "ProductName"]
      operating_system : string option; [@default None] [@key "OperatingSystem"]
      id : string; [@key "Id"]
    }
    [@@deriving jsont]

    let method' = Get
    let endpoint _ = [ "System"; "Info" ]
  end
end

(* Forward declaration to be filled by the app *)
let session_uuid = ref None
let set_session_uuid s = session_uuid := Some s

let authorization ?token () =
  let token =
    match token with None -> "" | Some t -> Printf.sprintf ", Token=%S" t
  in
  let session_uuid = Option.value ~default:"" !session_uuid in
  Printf.sprintf
    "MediaBrowser Client=\"Ocamix\", Device=\"Firefox\", DeviceId=\"%s\", \
     Version=\"0.1\"%s"
    session_uuid token

let request (type pp p r) ~base_url ?token ?headers
    (module Q : Query
      with type path_params = pp
       and type params = p
       and type response = r) (params : p) (path_params : pp) : r Fut.or_error =
  let open Brr_io.Fetch in
  let base_uri = Uri.v (Jstr.v base_url) in
  let base_path_segments = Result.get_exn @@ Uri.path_segments base_uri in
  let endpoint_path_segments = List.map ~f:Jstr.v (Q.endpoint path_params) in
  let path_segments =
    if Equal.poly base_path_segments [ Jstr.empty ] then endpoint_path_segments
    else List.concat [ base_path_segments; endpoint_path_segments ]
  in
  let uri = Uri.with_path_segments base_uri path_segments in
  let uri = Result.get_exn uri in
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
          params
          |> Jsont_brr.encode_jv Q.params_jsont
          |> Result.get_exn |> Uri.Params.of_obj
        in
        let uri_with_params = Uri.with_query_params uri params in
        (Request.init ~headers ~method' (), Uri.to_jstr uri_with_params)
    | Post ->
        let body =
          params
          |> Jsont_brr.encode Q.params_jsont
          |> Result.get_exn |> Body.of_jstr
        in
        (Request.init ~headers ~method' ~body (), Uri.to_jstr uri)
  in
  let open Fut.Result_syntax in
  let* res = request @@ Request.v ~init url in
  let+ json = Response.as_body res |> Body.json in
  let result = Jsont_brr.decode_jv Q.response_jsont json in
  match result with
  | Ok result -> result
  | Error e ->
      Console.error [ "An error occured while decoding response: "; json ];
      Console.error [ Jv.Error.message e ];
      raise (Jv.Error e)

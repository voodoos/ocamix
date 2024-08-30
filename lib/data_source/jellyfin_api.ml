open Brr

type method' = Get | Post

let jstr_of_method = function Get -> Jstr.v "GET" | Post -> Jstr.v "POST"

module Types = struct
  type order = Ascending | Descending [@@deriving yojson]

  type sort =
    | Album
    | AlbumArtist
    | Artist
    | Budget
    | CommunityRating
    | CriticRating
    | DateCreated
    | DatePlayed
    | PlayCount
    | PremiereDate
    | ProductionYear
    | SortName
    | Random
    | Revenue
    | Runtime
  [@@deriving yojson]
end

type user = {
  name : string; [@key "Name"]
  server_id : string; [@key "ServerId"]
  server_name : string option; [@default None] [@key "ServerName"]
  id : string; [@key "Id"]
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

module type Query = sig
  type path_params
  type params [@@deriving yojson]
  type response [@@deriving yojson]

  val method' : method'
  val endpoint : path_params -> string list
end

module Authenticate_by_name = struct
  type path_params = unit

  type params = { username : string; [@key "Username"] pw : string [@key "Pw"] }
  [@@deriving yojson]

  type response = {
    user : user; [@key "User"]
    access_token : string; [@key "AccessToken"]
    server_id : string; [@key "ServerId"]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let method' = Post
  let endpoint _ = [ "Users"; "AuthenticateByName" ]
end

module Item = struct
  type image_blur_hash = (string * string) list

  let image_blur_hash_of_yojson y =
    let assoc = Yojson.Safe.Util.to_assoc y in
    List.map (fun (key, v) -> (key, Yojson.Safe.Util.to_string v)) assoc

  let yojson_of_image_blur_hash i : Yojson.Safe.t =
    let assoc = List.map (fun (key, v) -> (key, `String v)) i in
    `Assoc assoc

  type image_blur_hashes = {
    primary : image_blur_hash option; [@yojson.option] [@key "Primary"]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

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
  [@@deriving yojson]

  type field =
    | AirTime
    | BasicSyncInfo
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
  [@@deriving yojson]

  (* The [Type] field is actually a json string but we want to see it as a
     variant (which is a list of one string) *)
  type type_str = type_

  let type_str_of_yojson j =
    let s = Yojson.Safe.Util.to_string j in
    type__of_yojson (`List [ `String s ])

  let yojson_of_type_str ts =
    match yojson_of_type_ ts with `List [ json ] -> json | _ -> assert false

  type t = {
    name : string; [@key "Name"]
    sort_name : string option; [@yojson.option] [@key "SortName"]
    id : string; [@key "Id"]
    path : string option; [@yojson.option] [@key "Path"]
    album_id : string option; [@yojson.option] [@key "AlbumId"]
    parent_id : string option option;
        (* [ParentId] might absent, [null], or a string *)
        [@yojson.option]
        [@key "ParentId"]
    server_id : string; [@key "ServerId"]
    image_blur_hashes : image_blur_hashes; [@key "ImageBlurHashes"]
    type_ : type_str; [@key "Type"]
    collection_type : string option;
        [@default None] [@yojson_drop_default ( = )] [@key "CollectionType"]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Items = struct
  type path_params = unit

  type params = {
    ids : string list; [@default []] [@yojson_drop_default ( = )] [@key "ids"]
    parent_id : string option; [@yojson.option] [@key "parentId"]
    user_id : string; [@key "userId"]
    fields : Item.field list;
    include_item_types : Item.type_ list; [@key "includeItemTypes"]
    start_index : int option; [@yojson.option] [@key "startIndex"]
    limit : int option; [@yojson.option]
    sort_order : Types.order option; [@yojson.option] [@key "sortOrder"]
    sort_by : Types.sort list; [@key "sortBy"]
    recursive : bool;
    enable_user_data : bool; [@key "enableUserData"]
    enable_images : bool; [@key "enableImages"]
  }
  [@@deriving yojson]

  type response = {
    items : Item.t list; [@key "Items"]
    total_record_count : int; [@key "TotalRecordCount"]
    start_index : int; [@key "StartIndex"]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let method' = Get
  let endpoint _ = [ "Items" ]
end

module Views = struct
  type path_params = { user_id : string }

  type params = {
    include_external_content : bool; [@key "includeExternalContent"]
  }
  [@@deriving yojson]

  type response = {
    items : Item.t list; [@key "Items"]
    total_record_count : int; [@key "TotalRecordCount"]
    start_index : int; [@key "StartIndex"]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let method' = Get
  let endpoint pp = [ "Users"; pp.user_id; "Views" ]
end

module Virtual_folders = struct
  type path_params = unit
  type params = unit [@@deriving yojson]

  type virtual_folder = {
    name : string; [@key "Name"]
    locations : string list; [@key "Locations"]
    item_id : string; [@key "ItemId"]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  type response = virtual_folder list [@@deriving yojson]

  let method' = Get
  let endpoint _ = [ "Library"; "VirtualFolders" ]
end

module System = struct
  module Info = struct
    type path_params = unit
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
  let base_path_segments = Result.get_ok @@ Uri.path_segments base_uri in
  let endpoint_path_segments = List.map Jstr.v (Q.endpoint path_params) in
  let path_segments =
    if base_path_segments = [ Jstr.empty ] then endpoint_path_segments
    else List.concat [ base_path_segments; endpoint_path_segments ]
  in
  let uri = Uri.with_path_segments base_uri path_segments in
  let uri = Result.get_ok uri in
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
  let yojson = Yojson.Safe.from_string (Jstr.to_string json) in
  try Q.response_of_yojson yojson
  with e ->
    Console.log [ "An error occured while decoding response: "; json ];
    Console.log [ e ];
    raise e

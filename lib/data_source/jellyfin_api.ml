open Std
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
  type genre_item = { name : string; [@key "Name"] id : string [@key "Id"] }
  [@@deriving jsont]

  type artist_item = { name : string; [@key "Name"] id : string [@key "Id"] }
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

  (* It seems like Jellyfin uses image "tags" to distinguished several versions
     of the same image. Blur hashes are associated to images via their tags.
     The tags are generated like this:

      public string
        GetImageCacheTag(string baseItemPath, DateTime imageDateModified)
        => (baseItemPath + imageDateModified.Ticks)
              .GetMD5().ToString("N", CultureInfo.InvariantCulture);
     See https://github.com/jellyfin/jellyfin/blob/84f66dd54e74621e4d81cd57648c4d27411d82d9/src/Jellyfin.Drawing/ImageProcessor.cs#L410
    *)

  type t = {
    name : string; [@key "Name"]
    sort_name : string option; [@option] [@key "SortName"]
    date_created : string option; [@option] [@key "DateCreated"]
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
    parent_index_number : int option; [@option] [@key "ParentIndexNumber"]
    index_number : int option; [@option] [@key "IndexNumber"]
    primary_image_aspect_ratio : float option;
        [@option] [@key "PrimaryImageAspectRatio"]
    image_tags : string String.Map.t;
        [@default String.Map.empty] [@key "ImageTags"]
    image_blur_hashes : string String.Map.t String.Map.t;
        [@default String.Map.empty] [@key "ImageBlurHashes"]
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
    enable_total_record_count : bool; [@key "enableTotalRecordCount"]
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
  type params = unit list

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

module Playback_info = struct
  type path_params = { item_id : string }

  (* [PlaybackInfoDto] *)
  type params = {
    user_id : string option; [@option] [@key "UserId"]
    max_streaming_bitrate : int option; [@option] [@key "MaxStreamingBitrate"]
    start_time_ticks : int64 option; [@option] [@key "StartTimeTicks"]
    audio_stream_index : int option; [@option] [@key "AudioStreamIndex"]
    subtitle_stream_index : int option; [@option] [@key "SubtitleStreamIndex"]
    max_audio_channels : int option; [@option] [@key "MaxAudioChannels"]
    media_source_id : string option; [@option] [@key "MediaSourceId"]
    live_stream_id : string option; [@option] [@key "LiveStreamId"]
    device_profile : Device_profile.t option; [@option] [@key "DeviceProfile"]
    auto_open_live_stream : bool option; [@option] [@key "AutoOpenLiveStream"]
    enable_direct_play : bool option; [@option] [@key "EnableDirectPlay"]
    enable_direct_stream : bool option; [@option] [@key "EnableDirectStream"]
    enable_transcoding : bool option; [@option] [@key "EnableTranscoding"]
    allow_audio_stream_copy : bool option;
        [@option] [@key "AllowAudioStreamCopy"]
    allow_video_stream_copy : bool option;
        [@option] [@key "AllowVideoStreamCopy"]
  }
  [@@deriving jsont]

  let params ?user_id ?max_streaming_bitrate ?start_time_ticks
      ?audio_stream_index ?subtitle_stream_index ?max_audio_channels
      ?media_source_id ?live_stream_id ?device_profile ?auto_open_live_stream
      ?enable_direct_play ?enable_direct_stream ?enable_transcoding
      ?allow_audio_stream_copy ?allow_video_stream_copy () =
    {
      user_id;
      max_streaming_bitrate;
      start_time_ticks;
      audio_stream_index;
      subtitle_stream_index;
      max_audio_channels;
      media_source_id;
      live_stream_id;
      device_profile;
      auto_open_live_stream;
      enable_direct_play;
      enable_direct_stream;
      enable_transcoding;
      allow_audio_stream_copy;
      allow_video_stream_copy;
    }

  (* Enums are kept as strings here: this is decoded from the server's answer
     and an unknown member would make the whole response fail to decode. *)
  type media_stream = {
    index : int option; [@option] [@key "Index"]
    type' : string option; [@option] [@key "Type"]
    codec : string option; [@option] [@key "Codec"]
    bit_rate : int option; [@option] [@key "BitRate"]
    channels : int option; [@option] [@key "Channels"]
    sample_rate : int option; [@option] [@key "SampleRate"]
    bit_depth : int option; [@option] [@key "BitDepth"]
    is_default : bool option; [@option] [@key "IsDefault"]
  }
  [@@deriving jsont]

  type media_source_info = {
    id : string option; [@option] [@key "Id"]
    name : string option; [@option] [@key "Name"]
    path : string option; [@option] [@key "Path"]
    etag : string option; [@option] [@key "ETag"]
    container : string option; [@option] [@key "Container"]
    size : int64 option; [@option] [@key "Size"]
    bitrate : int option; [@option] [@key "Bitrate"]
    run_time_ticks : int64 option; [@option] [@key "RunTimeTicks"]
    supports_direct_play : bool option; [@option] [@key "SupportsDirectPlay"]
    supports_direct_stream : bool option;
        [@option] [@key "SupportsDirectStream"]
    supports_transcoding : bool option; [@option] [@key "SupportsTranscoding"]
    supports_probing : bool option; [@option] [@key "SupportsProbing"]
    protocol : string option; [@option] [@key "Protocol"]
    media_streams : media_stream list; [@default []] [@key "MediaStreams"]
    required_http_headers : string String.Map.t;
        [@default String.Map.empty] [@key "RequiredHttpHeaders"]
    live_stream_id : string option; [@option] [@key "LiveStreamId"]
    transcoding_url : string option; [@option] [@key "TranscodingUrl"]
    transcoding_sub_protocol : string option;
        [@option] [@key "TranscodingSubProtocol"]
    transcoding_container : string option;
        [@option] [@key "TranscodingContainer"]
    read_at_native_framerate : bool option;
        [@option] [@key "ReadAtNativeFramerate"]
    is_remote : bool option; [@option] [@key "IsRemote"]
  }
  [@@deriving jsont]

  type response = {
    media_sources : media_source_info list; [@key "MediaSources"]
    play_session_id : string option; [@option] [@key "PlaySessionId"]
    error_code : string option; [@option] [@key "ErrorCode"]
    error_message : string option; [@option] [@key "ErrorMessage"]
  }
  [@@deriving jsont]

  let method' = Post
  let endpoint { item_id } = [ "Items"; item_id; "PlaybackInfo" ]
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

(** [uri_of_endpoint ~base_url segments] appends an endpoint's path segments to
    [base_url], preserving any path prefix the server is hosted under. *)
let uri_of_endpoint ~base_url segments =
  let base_uri = Uri.v (Jstr.v base_url) in
  let base_path_segments = Result.get_exn @@ Uri.path_segments base_uri in
  let endpoint_path_segments = List.map ~f:Jstr.v segments in
  let path_segments =
    if Equal.poly base_path_segments [ Jstr.empty ] then endpoint_path_segments
    else List.concat [ base_path_segments; endpoint_path_segments ]
  in
  Uri.with_path_segments base_uri path_segments |> Result.get_exn

let request (type pp p r) ~base_url ?token ?headers
    (module Q : Query
      with type path_params = pp
       and type params = p
       and type response = r) (params : p) (path_params : pp) : r Fut.or_error =
  let open Brr_io.Fetch in
  let uri = uri_of_endpoint ~base_url (Q.endpoint path_params) in
  let authorization = authorization ?token () in
  let headers =
    Headers.of_assoc ?init:headers
      Jstr.
        [
          (v "content-type", v "text/json"); (v "Authorization", v authorization);
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

open Std

(** Jellyfin's [DeviceProfile]: what this client is able to play. It is posted
    along with playback info requests so that the server can tell us whether a
    given media source can be streamed as is or has to be transcoded. *)

module Profile_type = struct
  type t = Audio | Video | Photo | Subtitle | Lyric [@@deriving jsont]
end

module Protocol = struct
  type t = Http [@key "http"] | Hls [@key "hls"] [@@deriving jsont]
end

module Context = struct
  type t = Streaming | Static [@@deriving jsont]
end

module Seek_info = struct
  type t = Auto | Bytes [@@deriving jsont]
end

module Codec_type = struct
  type t = Video | VideoAudio | Audio [@@deriving jsont]
end

module Condition = struct
  type type_ =
    | Equals
    | NotEquals
    | LessThanEqual
    | GreaterThanEqual
    | EqualsAny
  [@@deriving jsont]

  type property =
    | AudioChannels
    | AudioBitrate
    | AudioProfile
    | AudioSampleRate
    | AudioBitDepth
    | IsSecondaryAudio
    | NumAudioStreams
    | NumStreams
    | PacketLength
    | Width
    | Height
    | VideoBitDepth
    | VideoBitrate
    | VideoFramerate
    | VideoLevel
    | VideoProfile
    | VideoTimestamp
    | IsAnamorphic
    | RefFrames
    | NumVideoStreams
    | VideoCodecTag
    | IsAvc
    | IsInterlaced
    | VideoRangeType
    | VideoRotation
  [@@deriving jsont]

  type t = {
    condition : type_; [@key "Condition"]
    property : property; [@key "Property"]
    value : string; [@key "Value"]
    is_required : bool; [@default false] [@key "IsRequired"]
  }
  [@@deriving jsont]
end

type direct_play_profile = {
  container : string; [@key "Container"]
  audio_codec : string option; [@option] [@key "AudioCodec"]
  video_codec : string option; [@option] [@key "VideoCodec"]
  type' : Profile_type.t; [@key "Type"]
}
[@@deriving jsont]

let direct_play ?audio_codec ?video_codec ?(type' = Profile_type.Audio)
    container =
  { container; audio_codec; video_codec; type' }

type transcoding_profile = {
  container : string; [@key "Container"]
  type' : Profile_type.t; [@key "Type"]
  audio_codec : string; [@key "AudioCodec"]
  video_codec : string; [@default ""] [@key "VideoCodec"]
  protocol : Protocol.t; [@key "Protocol"]
  context : Context.t; [@key "Context"]
  max_audio_channels : string option; [@option] [@key "MaxAudioChannels"]
  min_segments : int; [@default 0] [@key "MinSegments"]
  segment_length : int; [@default 0] [@key "SegmentLength"]
  break_on_non_key_frames : bool option; [@option] [@key "BreakOnNonKeyFrames"]
  estimate_content_length : bool;
      [@default false] [@key "EstimateContentLength"]
  transcode_seek_info : Seek_info.t;
      [@default Seek_info.Auto] [@key "TranscodeSeekInfo"]
  copy_timestamps : bool; [@default false] [@key "CopyTimestamps"]
  enable_audio_vbr_encoding : bool;
      [@default true] [@key "EnableAudioVbrEncoding"]
  conditions : Condition.t list; [@default []] [@key "Conditions"]
}
[@@deriving jsont]

let transcoding ?(video_codec = "") ?max_audio_channels ?(min_segments = 0)
    ?(segment_length = 0) ?break_on_non_key_frames
    ?(estimate_content_length = false) ?(transcode_seek_info = Seek_info.Auto)
    ?(copy_timestamps = false) ?(enable_audio_vbr_encoding = true)
    ?(conditions = []) ?(type' = Profile_type.Audio)
    ?(context = Context.Streaming) ~protocol ~audio_codec container =
  {
    container;
    type';
    audio_codec;
    video_codec;
    protocol;
    context;
    max_audio_channels;
    min_segments;
    segment_length;
    break_on_non_key_frames;
    estimate_content_length;
    transcode_seek_info;
    copy_timestamps;
    enable_audio_vbr_encoding;
    conditions;
  }

type container_profile = {
  type' : Profile_type.t; [@key "Type"]
  container : string option; [@option] [@key "Container"]
  sub_container : string option; [@option] [@key "SubContainer"]
  conditions : Condition.t list; [@default []] [@key "Conditions"]
}
[@@deriving jsont]

type codec_profile = {
  type' : Codec_type.t; [@key "Type"]
  codec : string option; [@option] [@key "Codec"]
  container : string option; [@option] [@key "Container"]
  sub_container : string option; [@option] [@key "SubContainer"]
  conditions : Condition.t list; [@default []] [@key "Conditions"]
  apply_conditions : Condition.t list; [@default []] [@key "ApplyConditions"]
}
[@@deriving jsont]

type subtitle_profile = {
  format : string; [@key "Format"]
  method' : string; [@key "Method"] (* Encode|Embed|External|Hls|Drop *)
  container : string option; [@option] [@key "Container"]
  language : string option; [@option] [@key "Language"]
}
[@@deriving jsont]

type t = {
  name : string option; [@option] [@key "Name"]
  id : string option; [@option] [@key "Id"]
  max_streaming_bitrate : int option; [@option] [@key "MaxStreamingBitrate"]
  max_static_bitrate : int option; [@option] [@key "MaxStaticBitrate"]
  music_streaming_transcoding_bitrate : int option;
      [@option] [@key "MusicStreamingTranscodingBitrate"]
  max_static_music_bitrate : int option;
      [@option] [@key "MaxStaticMusicBitrate"]
  direct_play_profiles : direct_play_profile list;
      [@default []] [@key "DirectPlayProfiles"]
  transcoding_profiles : transcoding_profile list;
      [@default []] [@key "TranscodingProfiles"]
  container_profiles : container_profile list;
      [@default []] [@key "ContainerProfiles"]
  codec_profiles : codec_profile list; [@default []] [@key "CodecProfiles"]
  subtitle_profiles : subtitle_profile list;
      [@default []] [@key "SubtitleProfiles"]
}
[@@deriving jsont]

let empty =
  {
    name = None;
    id = None;
    max_streaming_bitrate = None;
    max_static_bitrate = None;
    music_streaming_transcoding_bitrate = None;
    max_static_music_bitrate = None;
    direct_play_profiles = [];
    transcoding_profiles = [];
    container_profiles = [];
    codec_profiles = [];
    subtitle_profiles = [];
  }

open Brr

type t = Jv.t

external of_jv : Jv.t -> t = "%identity"

let of_navigator n = Jv.get (Navigator.to_jv n) "mediaSession"

module Media_metadata = struct
  type img = { src : string; sizes : string; type' : string }

  let img_to_jv img =
    Jv.obj
      [|
        ("src", Jv.of_string img.src);
        ("sizes", Jv.of_string img.sizes);
        ("type", Jv.of_string img.type');
      |]

  let maybe_string jv prop =
    match Jv.find jv prop with None -> "unknown" | Some jv -> Jv.to_string jv

  let img_of_jv jv =
    let src = maybe_string jv "src" in
    let sizes = maybe_string jv "sizes" in
    let type' = maybe_string jv "type" in
    { src; sizes; type' }

  type t = {
    title : string;
    artist : string;
    album : string;
    artwork : img list;
  }

  let to_jv t =
    let artwork = Jv.of_list img_to_jv t.artwork in
    Jv.obj
      [|
        ("title", Jv.of_string t.title);
        ("artist", Jv.of_string t.artist);
        ("album", Jv.of_string t.album);
        ("artwork", artwork);
      |]

  let of_jv jv =
    let title = maybe_string jv "title" in
    let artist = maybe_string jv "artist" in
    let album = maybe_string jv "album" in
    let artwork =
      match Jv.find jv "artwork" with
      | None -> []
      | Some jv -> Jv.to_list img_of_jv jv
    in
    { title; artist; album; artwork }
end

let metadata t = Jv.get t "metadata" |> Jv.to_option Media_metadata.of_jv

let set_metadata t v =
  Console.log [ "Set metadata"; Media_metadata.to_jv v ];
  let metadata =
    Jv.new' (Jv.get Jv.global "MediaMetadata") [| Media_metadata.to_jv v |]
  in
  Jv.set t "metadata" metadata

module Action = struct
  type t = Jstr.t

  let next_track = Jstr.v "nexttrack"
  let previous_track = Jstr.v "previoustrack"
end

let set_action_handler t action f =
  let callback = Jv.callback ~arity:1 f in
  Jv.call t "setActionHandler" [| Jv.of_jstr action; callback |] |> ignore

let set_position_state ?duration ?playback_rate ?position t =
  let maybe_float name v = Option.map (fun v -> (name, Jv.of_float v)) v in
  let duration = maybe_float "duration" duration in
  let playback_rate = maybe_float "playbackRate" playback_rate in
  let position = maybe_float "position" position in
  let args =
    let state_dict =
      List.filter_map Fun.id [ duration; playback_rate; position ]
    in
    match state_dict with [] -> [||] | sd -> [| Jv.obj (Array.of_list sd) |]
  in
  Jv.call t "setPositionState" args |> ignore

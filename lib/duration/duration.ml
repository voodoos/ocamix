(* Jellyfin durations are in 10^-7 seconds... *)
(* Todo there are probably many libraries better than these hacks *)

type t = float [@@deriving yojson, jsont]
type time = { days : int; hours : int; minutes : int; seconds : int }

let to_seconds t = t /. 10000000.

let to_time t =
  let seconds = Int.of_float (to_seconds t) in
  let minutes = seconds / 60 in
  let hours = minutes / 60 in
  let minutes = minutes mod 60 in
  let days = hours / 24 in
  let hours = hours mod 60 in
  let seconds = seconds mod 60 in
  { days; hours; minutes; seconds }

let pp_track_time { days = _; hours; minutes; seconds } =
  if hours = 0 then Printf.sprintf "%02i:%02i" minutes seconds
  else Printf.sprintf "%02i:%02i:%02i" hours minutes seconds

let pp_track_duration t = pp_track_time (to_time t)

let pp_approx_time { days; hours; minutes; seconds } =
  if days > 0 then Printf.sprintf "%i days" days
  else if hours = 0 then Printf.sprintf "%02i:%02i" minutes seconds
  else Printf.sprintf "%02ih%02i" hours minutes

let pp_approx_duration t = pp_approx_time (to_time t)

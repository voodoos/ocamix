open! Import
open Brr
open Brr_lwd

type t = Elwd.t

let now_playing = Lwd.var None

let make () =
  let src =
    Lwd.get now_playing
    |> Lwd.map ~f:(fun now ->
           let url = Option.value ~default:"" now in
           At.src (Jstr.v url))
  in
  Elwd.audio
    ~at:
      [
        `P (At.v (Jstr.v "controls") (Jstr.v "true"));
        `P (At.v (Jstr.v "autoplay") (Jstr.v "true"));
        `P (At.v (Jstr.v "preload") (Jstr.v "auto"));
        `R src;
      ]
    []

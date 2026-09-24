open Brr

let () = El.append_children (Document.body G.document) [ El.txt' "Pouet" ]
let stream = Audio_stream.init ()
let () = Audio_stream.queue_song stream "/audio_test/1-to-10.ogg"
let () = Audio_stream.queue_song stream "/audio_test/10-to-0.mp3"

let () =
  Audio_stream.queue_song stream
    "/audio_test/Amyl and the Sniffers - Giddy Up - 03 Mandalay.ogg"

open! Fut.Syntax

let _ =
  let result = Audio_stream.resume stream in
  Console.log [ result ]

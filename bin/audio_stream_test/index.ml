open Brrer
open Brr

let () = El.append_children (Document.body G.document) [ El.txt' "Pouet" ]
let controls, stream = Audio_player.make_player ()
let () = Audio_stream.queue_song stream "/audio_test/1-to-10.ogg"
let () = Audio_stream.queue_song stream "/audio_test/10-to-0.mp3"

let () =
  Audio_stream.queue_song stream
    "/audio_test/Amyl and the Sniffers - Giddy Up - 03 Mandalay.ogg"

(*let el_ns ~d name =
  Jv.call (Document.to_jv d) "createElementNS"
    [| Jv.of_string "http://www.w3.org/2000/svg"; Jv.of_string name |]
  |> El.of_jv

let path ?c ?d ?fill children =
  let el = el_ns ~d:G.document "path" in
  El.set_at (Jstr.v "d") (Option.map Jstr.v d) el;
  El.set_at (Jstr.v "fill") (Option.map Jstr.v fill) el;
  Option.iter (fun c -> El.set_class (Jstr.v c) true el) c;
  El.append_children el children;
  el

let svg ?c ?(d = G.document) children =
  let el = el_ns ~d "svg" in
  Option.iter (fun c -> El.set_class (Jstr.v c) true el) c;
  El.append_children el children;
  el

(*document.createElementNS("http://www.w3.org/2000/svg", "rect")*)

let bar height =
  (* let height = 5. in *)
  let width = 0.75 in
  let b_height = 0.75 in
  Format.sprintf
    {|m 0,1
      l 0,%f
       c 0,%f %f,%f %f,0
       l 0,%f
       c 0,-%f -%f,-%f -%f,0
       z|}
    height b_height width b_height width height b_height width b_height width
*)

let _ =
  let on_load _ =
    let app = Lwd.observe controls in
    let on_invalidate _ =
      ignore @@ G.request_animation_frame
      @@ fun _ -> ignore @@ Lwd.quick_sample app
    in
    El.append_children (Document.body G.document) [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

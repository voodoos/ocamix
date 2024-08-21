open Brr

type t = Jv.t

external of_jv : Jv.t -> t = "%identity"

module Entry = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"

  type box_size = { block_size : int; inline_size : int }

  let box_size_of_jv jv =
    {
      block_size = Jv.get jv "blockSize" |> Jv.to_int;
      inline_size = Jv.get jv "inlineSize" |> Jv.to_int;
    }

  let border_box_size t = Jv.get t "borderBoxSize" |> Jv.to_array box_size_of_jv

  let content_box_size t =
    Jv.get t "contentBoxSize" |> Jv.to_array box_size_of_jv

  let content_rect t = Jv.get t "contentRect" |> Dom_rect_read_only.of_jv

  let device_pixel_content_box_size t =
    Jv.get t "devicePixelContentBoxSize" |> Jv.to_array box_size_of_jv

  let target t = Jv.get t "target" |> El.of_jv
end

type box = Content_box | Border_box | Device_pixel_content_box

let string_of_box = function
  | Content_box -> "content-box"
  | Border_box -> "border-box"
  | Device_pixel_content_box -> "device-pixel-content-box"

let disconnect t = ignore @@ Jv.call t "disconnect" [||]

let observe t ?box target =
  let args =
    let target = El.to_jv target in
    match box with
    | None -> [| target |]
    | Some b ->
        let box = Jv.obj [| ("box", Jv.of_string (string_of_box b)) |] in
        [| target; box |]
  in
  Jv.call t "observe" args |> ignore

let unobserve t target =
  let target = El.to_jv target in
  ignore @@ Jv.call t "disconnect" [| target |]

let create ~callback =
  let callback entries observer =
    let entries = Jv.to_list Entry.of_jv entries in
    callback entries (of_jv observer)
  in
  let args = [| Jv.repr callback |] in
  Jv.new' (Jv.get Jv.global "ResizeObserver") args

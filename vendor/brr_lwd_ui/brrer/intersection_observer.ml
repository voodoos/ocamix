open Brr

type t = Jv.t

external of_jv : Jv.t -> t = "%identity"

module Entry = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"

  let is_intersecting t = Jv.get t "isIntersecting" |> Jv.to_bool
  let target t = Jv.get t "target" |> El.of_jv
end

let observe t target = Jv.call t "observe" [| El.to_jv target |] |> ignore

let make_options ?root () =
  let assoc name to_jv v = (name, to_jv v) in
  let options = [ Option.map (assoc "root" El.to_jv) root ] in
  let options = List.filter_map Fun.id options in
  Jv.obj (Array.of_list options)

let create ~callback ?root () =
  let options = make_options ?root () in
  let callback entries observer =
    let entries = Jv.to_list Entry.of_jv entries in
    callback entries (of_jv observer)
  in
  let args = [| Jv.repr callback; options |] in
  Jv.new' (Jv.get Jv.global "IntersectionObserver") args

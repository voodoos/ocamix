type t = Jv.t

external of_jv : Jv.t -> t = "%identity"

let x t = Jv.get t "x" |> Jv.to_int
let y t = Jv.get t "y" |> Jv.to_int
let width t = Jv.get t "width" |> Jv.to_int
let height t = Jv.get t "height" |> Jv.to_int
let top t = Jv.get t "top" |> Jv.to_int
let right t = Jv.get t "right" |> Jv.to_int
let bottom t = Jv.get t "bottom" |> Jv.to_int
let left t = Jv.get t "left" |> Jv.to_int

open Brr_lwd_ui.Blur_hash
open Bimage

let test_hash = "LGF5]+Yk^6#M@-5c,1J5@[or[Q6."
let width = 360
let height = 200
let data = decode ~width ~height test_hash
let image = Image.of_data Color.rgba width height data
let _ = Bimage_unix.Stb.write_png "test.png" image

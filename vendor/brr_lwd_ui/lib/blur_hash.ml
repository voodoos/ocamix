module Caml_base64 = Base64
open! Brr

(* This implementation of a decoder for [BlurHash](https://blurha.sh/) follows
   closely the reference typescript implementation distributed under the MIT
   license:

   https://github.com/woltapp/blurhash/blob/master/TypeScript/src/decode.ts

   Some comments are copied from the design doc:
   https://github.com/woltapp/blurhash/blob/master/Algorithm.md
*)

module Base83 = struct
  let alphabet =
    let table = Hashtbl.create 83 in
    String.iteri
      (fun i c -> Hashtbl.add table c i)
      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~";
    table

  let decode_char c = Hashtbl.find alphabet c

  let decode s =
    String.fold_left
      (fun acc c ->
        let index = decode_char c in
        (acc * 83) + index)
      0 s
end

let sRGB_to_linear i =
  let v = Float.of_int i /. 255. in
  if v <= 0.04045 then v /. 12.92 else Float.pow ((v +. 0.055) /. 1.055) 2.4

let linear_to_sRGB v =
  let v = Float.(max 0. (min 1. v)) in
  if v <= 0.0031308 then Float.to_int ((v *. 12.92 *. 255.) +. 0.5)
  else
    Float.to_int ((((1.055 *. Float.pow v (1. /. 2.4)) -. 0.055) *. 255.) +. 0.5)

let decode_dc value =
  let r = value lsr 16 in
  let g = (value lsr 8) land 255 in
  let b = value land 255 in
  (sRGB_to_linear r, sRGB_to_linear g, sRGB_to_linear b)

let sign_pow x y =
  let result = Float.pow x y in
  if x < 0. then -1. *. result else result

let decode_ac max_val value =
  let open Float in
  let quant_r = floor (value /. (19. *. 19.)) in
  let quant_g = to_int (floor (value /. 19.)) mod 19 |> of_int in
  let quant_b = to_int value mod 19 |> of_int in
  let f q = max_val *. sign_pow ((q -. 9.) /. 9.) 2.0 in
  (f quant_r, f quant_g, f quant_b)

let decode ?(punch = 1.) ~width ~height s =
  let n_comps = String.get s 0 |> Base83.decode_char in
  (* For a BlurHash with nx components along the X axis and ny components along
     the Y axis, this is equal to (nx - 1) + (ny - 1) * 9. *)
  let n_comps_x = (n_comps mod 9) + 1 in
  let n_comps_y = Float.(floor (of_int n_comps /. 9.) |> to_int) + 1 in

  let max_ac_comp_value = String.get s 1 |> Base83.decode_char in
  (* All AC components are scaled by this value. It represents a floating-point
     value of (max + 1) / 166. *)

  let max_value = Float.of_int (max_ac_comp_value + 1) /. 166. in
  let decode_ac = decode_ac (max_value *. punch) in

  let colors =
    Array.init (n_comps_x * n_comps_y) @@ function
    | 0 ->
        (* The average colour of the image in sRGB space, encoded as a 24-bit
           RGB value, with R in the most significant position. This value can be
           used directly if you only want the average colour rather than the
           full DCT-encoded image.*)
        let value = Base83.decode (String.sub s 2 4) in
        decode_dc value
    | i ->
        (* The AC components of the DCT transform, ordered by increasing X
           first, then Y. They are encoded as three values for R, G and B, each
           between 0 and 18. They are combined together as R * 19^2 + G * 19 +
           B, for a total range of 0 to 6859.

           Each value represents a floating-point value between -1 and 1. 0-8
           represent negative values, 9 represents zero, and 10-18 represent
           positive values. Positive values are encoded as ((X - 9) / 9) ^ 2,
           while negative values are encoded as -((9 - X) / 9 ) ^ 2. ^
           represents exponentiation. This value is then multiplied by the
           maximum AC component value, field 2 above. *)
        let start = 4 + (i * 2) in
        let value = Base83.decode (String.sub s start 2) in
        decode_ac (Float.of_int value)
  in

  let bytes_per_row = width * 4 in
  let pixels =
    Bigarray.(Array1.create int8_unsigned c_layout (bytes_per_row * height))
  in
  let clamp i = if i < 0 then 0 else if i > 255 then 255 else i in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let r = ref 0. in
      let g = ref 0. in
      let b = ref 0. in

      for j = 0 to n_comps_y - 1 do
        let basis_y =
          Float.(cos (pi *. of_int y *. of_int j /. of_int height))
        in
        for i = 0 to n_comps_x - 1 do
          let basis =
            Float.(cos (pi *. of_int x *. of_int i /. of_int width) *. basis_y)
          in
          let r', g', b' = colors.(i + (j * n_comps_x)) in
          r := !r +. (r' *. basis);
          g := !g +. (g' *. basis);
          b := !b +. (b' *. basis)
        done
      done;

      let r = linear_to_sRGB !r in
      let g = linear_to_sRGB !g in
      let b = linear_to_sRGB !b in
      pixels.{(4 * x) + 0 + (y * bytes_per_row)} <- clamp r;
      pixels.{(4 * x) + 1 + (y * bytes_per_row)} <- clamp g;
      pixels.{(4 * x) + 2 + (y * bytes_per_row)} <- clamp b;
      pixels.{(4 * x) + 3 + (y * bytes_per_row)} <- 255
    done
  done;
  pixels

open Brrer.Brr

let to_canvas ~width ~height blur_hash =
  let open Brr_canvas in
  let el = El.canvas ~at:[ At.width width; At.height height ] [] in
  let canvas = Canvas.of_el el in
  let context = C2d.get_context canvas in
  let pixels = Blur_hash.decode ~width ~height blur_hash in
  let data = Tarray.of_bigarray1 pixels in
  let data =
    (* TODO: this makes a copy, which is sad.

       When called with an instance of a TypedArray subclass, the typedArray
       gets copied into a new typed array. For a non-bigint TypedArray
       constructor, the typedArray parameter can only be of one of the
       non-bigint types (such as Int32Array). Similarly, for a bigint TypedArray
       constructor (BigInt64Array or BigUint64Array), the typedArray parameter
       can only be of one of the bigint types. Each value in typedArray is
       converted to the corresponding type of the constructor before being
       copied into the new array. The length of the new typed array will be same
       as the length of the typedArray argument. *)
    Tarray.(of_tarray Uint8_clamped data)
  in
  let image_data = C2d.Image_data.create ~data ~w:width ~h:height () in
  C2d.put_image_data context image_data ~x:0 ~y:0;
  el

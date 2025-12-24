open! Std
open Brrer.Brr
open Blur_hashes_worker_api

module Worker () = struct
  let on_query (type a b) (q : (a, b) query) (params : a) :
      (b, error) Fut.result =
    match q with
    | Render ->
        let { hash; w; h } = params in
        let pixels = Brr_lwd_ui.Blur_hash.decode ~width:w ~height:h hash in
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
        Fut.ok data
end

include Make_worker (Worker)

include ContainersLabels
module Brr_utils = Brr_utils

module Set = struct
  include Set

  module type OrderedTypeJsont = sig
    type t

    include OrderedType with type t := t

    val jsont : t Jsont.t
  end

  module Make (C : OrderedTypeJsont) = struct
    include Make (C)

    let jsont =
      let enc =
        {
          Jsont.Array.enc =
            (fun f init set ->
              let i = ref (-1) in
              fold
                (fun elt acc ->
                  incr i;
                  f acc !i elt)
                set init);
        }
      in
      Jsont.Array.map ~enc
        ~dec_empty:(fun () -> empty)
        ~dec_add:(fun _ -> add)
        ~dec_finish:(fun _ _ set -> set)
        C.jsont
      |> Jsont.Array.array
  end
end

module Int = struct
  module T = struct
    include Int

    let jsont = Jsont.int
  end

  include T
  module Set = Set.Make (T)

  module Map = struct
    include Map.Make (T)

    let jsont elt_jsont =
      let enc =
        {
          Jsont.Array.enc =
            (fun f init map -> fold (fun i elt acc -> f acc i elt) map init);
        }
      in
      Jsont.Array.map ~enc
        ~dec_empty:(fun () -> empty)
        ~dec_add:add
        ~dec_finish:(fun _ _ map -> map)
        elt_jsont
      |> Jsont.Array.array
  end
end

module String = struct
  module T = struct
    include String

    let jsont = Jsont.string
  end

  include T
  module Set = Set.Make (T)

  module Map = struct
    include Map.Make (T)

    let string_map ?kind ?doc type' =
      let dec_empty () = empty in
      let dec_add _meta n v mems = add n v mems in
      let dec_finish _meta mems = mems in
      let enc f mems acc =
        fold (fun n v acc -> f Jsont.Meta.none n v acc) mems acc
      in
      Jsont.Object.Mems.map ?kind ?doc type' ~dec_empty ~dec_add ~dec_finish
        ~enc:{ enc }

    let jsont t =
      let open Jsont.Object in
      map Fun.id |> keep_unknown (string_map t) ~enc:Fun.id |> finish
  end
end

module Encodings = struct
  let to_jstr t = Jv.repr t |> Brr.Json.encode
  let to_jv t = to_jstr t |> Jv.of_jstr

  let of_jstr jstr =
    match Brr.Json.decode jstr with
    | Ok v -> Ok (Obj.magic v)
    | Error err ->
        Brr.Console.error [ err ];
        Error (`Msg "Failed to unmarshal data")

  let of_jv jv = Jv.to_jstr jv |> of_jstr

  let set_jsont elt_jsont =
    let enc =
      {
        Jsont.Array.enc =
          (fun f init set ->
            let i = ref (-1) in
            Int.Set.fold
              (fun elt acc ->
                incr i;
                f acc !i elt)
              set init);
      }
    in
    Jsont.Array.map ~enc
      ~dec_empty:(fun () -> Int.Set.empty)
      ~dec_add:(fun _ -> Int.Set.add)
      ~dec_finish:(fun _ _ set -> set)
      elt_jsont
    |> Jsont.Array.array
end

let random_state = Random.get_state ()
let new_uuid_v4 () = Uuidm.v4_gen random_state ()

(** [tee f x] applies [f] to [x] and returns [x] *)
let tee f x =
  let () = f x in
  x

(* !! Changes to this function requires database upgrades. Std is probably not
   the right place for this kind of app-specific behavior.*)
let canonicalize_string s =
  Ubase.from_utf8 ?malformed:None ~strip:"_" s
  |> String.lowercase_ascii
  |> String.filter ~f:(fun c ->
      let c = Char.to_int c in
      (c >= 97 && c <= 122) || (c >= 48 && c <= 57))

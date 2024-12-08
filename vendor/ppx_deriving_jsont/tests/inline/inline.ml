(* Variants *)

type sort = A | B | V [@key "C"] [@@deriving_inline jsont]

let _ = fun (_ : sort) -> ()
let sort_jsont = Jsont.enum ~kind:"Sort" [ ("A", A); ("B", B); ("C", V) ]
let _ = sort_jsont

[@@@deriving.end]

let () =
  let encoded = Jsont_bytesrw.encode_string sort_jsont V |> Result.get_ok in
  assert (String.equal {|"C"|} encoded)

let () =
  let decoded =
    Jsont_bytesrw.decode_string sort_jsont {|"C"|} |> Result.get_ok
  in
  assert (decoded = V)

(* Records *)

type t = {
  name : string;
  maybe_name : string option; [@option]
  ids : string list; [@default []] [@omit List.is_empty]
  sort : sort; [@key "Sort"]
}
[@@deriving_inline jsont]

let _ = fun (_ : t) -> ()

let jsont =
  Jsont.Object.finish
    (Jsont.Object.mem "Sort" sort_jsont
       ~enc:(fun t -> t.sort)
       ?dec_absent:None ?enc_omit:None
       (Jsont.Object.mem "ids" (Jsont.list Jsont.string)
          ~enc:(fun t -> t.ids)
          ?dec_absent:(Some []) ?enc_omit:(Some List.is_empty)
          (Jsont.Object.mem "maybe_name"
             (Jsont.option Jsont.string)
             ~enc:(fun t -> t.maybe_name)
             ?dec_absent:(Some None) ?enc_omit:(Some Option.is_none)
             (Jsont.Object.mem "name" Jsont.string
                ~enc:(fun t -> t.name)
                ?dec_absent:None ?enc_omit:None
                (Jsont.Object.map ~kind:"T" (fun name ->
                     fun maybe_name ->
                      fun ids -> fun sort -> { name; maybe_name; ids; sort }))))))

let _ = jsont

[@@@deriving.end]

(* FIXME there is an issue with ppx inline: 

Error: ppxlib: the corrected code doesn't round-trip.
This is probably a bug in the OCaml printer:
1265c1265,1291
<                                               (pexp_attributes ()))))
---
>                                               (pexp_attributes
>                                                (((attr_name
>                                                   ((txt
>                                                     ppxlib.migration.stop_taking)
>                                                    (loc
>                                                     ((loc_start
>                                                       ((pos_fname _none_)
>                                                        (pos_lnum 1)
>                                                        (pos_bol 0)
>                                                        (pos_cnum -1)))
>                                                      (loc_end
>                                                       ((pos_fname _none_)
>                                                        (pos_lnum 1)
>                                                        (pos_bol 0)
>                                                        (pos_cnum -1)))
>                                                      (loc_ghost true)))))
>                                                  (attr_payload (PStr ()))
>                                                  (attr_loc
>                                                   ((loc_start
>                                                     ((pos_fname _none_)
>                                                      (pos_lnum 1) (pos_bol 0)
>                                                      (pos_cnum -1)))
>                                                    (loc_end
>                                                     ((pos_fname _none_)
>                                                      (pos_lnum 1) (pos_bol 0)
>                                                      (pos_cnum -1)))
>                                                    (loc_ghost true)))))))))
1277c1303,1327
<                                            (pexp_attributes ()))))
---
>                                            (pexp_attributes
>                                             (((attr_name
>                                                ((txt
>                                                  ppxlib.migration.stop_taking)
>                                                 (loc
>                                                  ((loc_start
>                                                    ((pos_fname _none_)
>                                                     (pos_lnum 1) (pos_bol 0)
>                                                     (pos_cnum -1)))
>                                                   (loc_end
>                                                    ((pos_fname _none_)
>                                                     (pos_lnum 1) (pos_bol 0)
>                                                     (pos_cnum -1)))
>                                                   (loc_ghost true)))))
>                                               (attr_payload (PStr ()))
>                                               (attr_loc
>                                                ((loc_start
>                                                  ((pos_fname _none_)
>                                                   (pos_lnum 1) (pos_bol 0)
>                                                   (pos_cnum -1)))
>                                                 (loc_end
>                                                  ((pos_fname _none_)
>                                                   (pos_lnum 1) (pos_bol 0)
>                                                   (pos_cnum -1)))
>                                                 (loc_ghost true)))))))))
1287c1337,1361
<                                         (pexp_attributes ()))))
---
>                                         (pexp_attributes
>                                          (((attr_name
>                                             ((txt
>                                               ppxlib.migration.stop_taking)
>                                              (loc
>                                               ((loc_start
>                                                 ((pos_fname _none_)
>                                                  (pos_lnum 1) (pos_bol 0)
>                                                  (pos_cnum -1)))
>                                                (loc_end
>                                                 ((pos_fname _none_)
>                                                  (pos_lnum 1) (pos_bol 0)
>                                                  (pos_cnum -1)))
>                                                (loc_ghost true)))))
>                                            (attr_payload (PStr ()))
>                                            (attr_loc
>                                             ((loc_start
>                                               ((pos_fname _none_)
>                                                (pos_lnum 1) (pos_bol 0)
>                                                (pos_cnum -1)))
>                                              (loc_end
>                                               ((pos_fname _none_)
>                                                (pos_lnum 1) (pos_bol 0)
>                                                (pos_cnum -1)))
>                                              (loc_ghost true)))))))))
*)

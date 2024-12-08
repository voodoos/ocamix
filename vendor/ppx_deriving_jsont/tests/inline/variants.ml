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

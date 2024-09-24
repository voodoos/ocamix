(lang dune 3.11)

(generate_opam_files)

(source
 (github voodoos/ocamix))

(license AGPL-3.0)

(authors "Ulysse Gérard")

(maintainers "")

(package
 (allow_empty)
 (name ocamix)
 (synopsis "A playlist generator")
 (description "A _really_ long description")
 (depends
  containers
  containers-data
  js_of_ocaml
  js_of_ocaml-ppx
  brr
  lwd
  brr-lwd
  yojson
  ppx_yojson_conv
  ezjsonm
  json-data-encoding
  json-data-encoding-browser
  uuidm
  base64
  hector))

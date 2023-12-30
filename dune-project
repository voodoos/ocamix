(lang dune 3.11)

(generate_opam_files)

(source
 (github voodoos/ocamix))

(license AGPL-3.0)

(authors "Ulysse G\195\169rard")

(maintainers "")

(package
 (allow_empty)
 (name ocamix)
 (synopsis "A playlist generator")
 (description "A _really_ long description")
 (depends
  containers
  js_of_ocaml
  js_of_ocaml-ppx
  brr
  lwd
  brr-lwd
  yojson
  jsonxt
  ppx_yojson_conv
  base64))

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
  bytesrw
  (jsont
   (>= "0.1.1"))
  ppx_deriving_jsont
  uuidm
  base64
  hector
  ubase
  (bimage :with-tests)
  (bimage-unix :with-tests)))

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
 (depends containers js_of_ocaml brr lwd brr-lwd yojson ppx_yojson_conv))

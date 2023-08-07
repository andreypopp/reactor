(lang dune 3.9)

(using melange 0.1)

(name react)

(generate_opam_files true)

(source
 (github andreypopp/react_of_ocaml))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (allow_empty)
 (name react)
 (synopsis "React for OCaml")
 (depends ocaml dune lwt yojson containers dream melange ppxlib ppx_yojson_conv
          (ocaml-lsp-server :dev) (ocamlformat :dev))
 (tags
  (dream reactjs)))

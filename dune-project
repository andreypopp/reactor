(lang dune 3.9)

(using melange 0.1)

(name react)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (allow_empty)
 (name react)
 (synopsis "React for OCaml")
 (depends ocaml dune lwt yojson containers dream)
 (tags
  (dream reactjs)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

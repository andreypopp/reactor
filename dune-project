(lang dune 3.9)

(name react_server_components)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name react_server_components)
 (synopsis "React Server Components for OCaml")
 (description "Allow to define React Server Components in OCaml")
 (depends ocaml dune lwt containers dream)
 (tags
  (dream reactjs)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

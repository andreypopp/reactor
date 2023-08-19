(lang dune 3.9)

(using melange 0.1)

(using dune_site 0.1)

(name react)

(generate_opam_files true)

(source
 (github andreypopp/react_of_ocaml))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name remote)
 (synopsis "Typed remote APIs for Melange and Dream")
 (depends
  ocaml
  dune
  lwt
  yojson
  containers
  dream
  melange
  ppxlib
  ppx_yojson_conv
  (ocaml-lsp-server :dev)
  (ocamlformat :dev))
 (tags
  (dream reactjs melange)))

(package
 (allow_empty)
 (name react)
 (synopsis "React for OCaml")
 (depends
  ocaml
  dune
  lwt
  yojson
  containers
  dream
  melange
  ppxlib
  ppx_yojson_conv
  (ocaml-lsp-server :dev)
  (ocamlformat :dev))
 (tags
  (dream reactjs)))

(package
 (name react_example)
 (sites
  (share static))
 (depends
  ocaml
  dune
  lwt
  yojson
  containers
  dream
  melange
  dune-site
  ppx_yojson_conv
  (ocaml-lsp-server :dev)
  (ocamlformat :dev)))

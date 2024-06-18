(lang dune 3.16)
(using dune_site 0.1)
(using melange 0.1)

(name reactor)


(generate_opam_files true)

(source
 (github andreypopp/reactor))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(package
 (name htmlgen)
 (depends
  (ocaml
   (>= 4.14))
  dune
  containers))

(package
 (name realm)
 (depends
  (ocaml
   (>= 4.14))
  melange
  dune
  lwt
  yojson))

(package
 (name remote)
 (depends
  (ocaml
   (>= 4.14))
  melange
  dune
  melange-fetch
  containers
  ppxlib
  htmlgen
  hmap
  realm
  ppx_deriving_router
  yojson))

(package
 (name react_server)
 (depends
  (ocamlformat :with-test)
  (ocaml
   (>= 4.14))
  dune
  remote
  realm
  htmlgen
  containers
  lwt
  ppxlib
  yojson))

(package
 (name react_browser)
 (depends
  (ocamlformat :with-test)
  (ocaml
   (>= 4.14))
  melange
  dune
  reason-react
  remote
  realm
  ppxlib))

(package
 (name react_dream)
 (depends
  (ocaml
   (>= 4.14))
  dune
  lwt
  dream
  htmlgen
  react_server))

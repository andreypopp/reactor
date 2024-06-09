(lang dune 3.15)
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
  hmap
  realm
  ppx_deriving_router
  yojson))

(package
 (name react_server)
 (depends
  (ocaml
   (>= 4.14))
  dune
  remote
  realm
  containers
  lwt
  ppxlib
  yojson))

(package
 (name react_browser)
 (depends
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
  react_server))

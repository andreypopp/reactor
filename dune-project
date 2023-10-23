(lang dune 3.11)

(using melange 0.1)

(using dune_site 0.1)

(generate_opam_files true)

(source
 (github andreypopp/react_of_ocaml))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(package
 (name react))

(package
 (name react_example)
 (sites
  (share static)))

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (format
   (run ocamlformat-mlx %{input-file}))
  (preprocess
   (run mlx-pp %{input-file}))))

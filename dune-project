(lang dune 3.11)

(using melange 0.1)

(using dune_site 0.1)

(name react)

(generate_opam_files false)

(source
 (github andreypopp/react_of_ocaml))

(authors "Andrey Popp")

(maintainers "Andrey Popp")

(license LICENSE)

(documentation https://url/to/documentation)

(package 
  (name remote))

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
  (format (run ocamlformat-mlx %{input-file}))
  (preprocess
   (run mlx-pp %{input-file}))))

init:
	@opam switch create . "5.1.0" --no-install -y
	@opam pin add "dune.3.11.0" "git+https://github.com/andreypopp/dune.git#main" --no-action -y
	@opam pin add "mlx.~dev" "https://github.com/andreypopp/mlx/archive/refs/heads/main.zip" --no-action -y
	@opam pin add "ocamlmerlin-mlx.~dev" "https://github.com/andreypopp/mlx/archive/refs/heads/main.zip" --no-action -y
	@opam pin add "ocamlformat-mlx-lib.~dev" "https://github.com/andreypopp/ocamlformat-mlx/archive/refs/heads/main.zip" --no-action -y
	@opam pin add "ocamlformat-mlx.~dev" "https://github.com/andreypopp/ocamlformat-mlx/archive/refs/heads/main.zip" --no-action -y
	@opam install . --deps-only -y
	@pnpm install

build:
	dune build

watch:
	dune build --watch

test:
	dune runtest

start:
	@DUNE_CONFIG__GLOBAL_LOCK=disabled watchexec --restart --no-ignore \
		--watch $$OPAMSWITCH/_build/install/default/bin --filter 'react-example' -- \
		dune exec --no-build -- react-example

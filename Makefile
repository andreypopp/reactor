init:
	@opam switch create . "5.1.0" --no-install -y
	@opam repo add andreypopp https://github.com/andreypopp/opam-repository.git
	@opam pin add "dune.3.11.0" "git+https://github.com/andreypopp/dune.git#main" --no-action -y
	@opam install ./realm ./remote ./ppx_deriving_schema ./ppx_deriving_json ./react ./react_dream ./ --deps-only -y
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

init:
	@opam switch create . 4.14.1 -y --deps-only
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

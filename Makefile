OPAM_PACKAGES=$(wildcard ./*.opam) $(wildcard ./*/*.opam)

init: opam-switch opam-install pnpm-install

opam-switch:
	opam switch create . "5.1.1" --no-install -y
	opam repo add andreypopp https://github.com/andreypopp/opam-repository.git

opam-install:
	opam install $(OPAM_PACKAGES) --deps-only -y

pnpm-install:
	pnpm install

build:
	dune build

watch:
	dune build --watch

fmt:
	dune fmt

test:
	dune runtest

start:
	@DUNE_CONFIG__GLOBAL_LOCK=disabled watchexec --restart --no-ignore \
		--watch $$OPAMSWITCH/_build/install/default/bin --filter 'react-example' -- \
		dune exec --no-build -- react-example

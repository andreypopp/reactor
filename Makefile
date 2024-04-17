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

react-example-ssr:
	dune exec -- react-example-ssr

react-example-server-only:
	dune exec -- react-example-server-only

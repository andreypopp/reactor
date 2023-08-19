init:
	@opam switch create . 5.1.0~rc1 --no-install
	@opam pin add "yojson.dev" ./deps/yojson --no-action
	@opam pin add "ppx_yojson_conv.dev" ./deps/ppx_yojson_conv --no-action
	@opam pin add "ppx_yojson_conv_lib.dev" ./deps/ppx_yojson_conv_lib --no-action
	@opam pin add "melange.dev" ./deps/melange --no-action
	@opam pin add "ocaml-lsp-server.dev" ./deps/ocaml-lsp --no-action
	@opam install . --deps-only
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

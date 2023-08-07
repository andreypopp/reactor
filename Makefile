init:
	@opam switch create . 4.14.1 -y --deps-only
	@pnpm install

start:
	@watchexec --restart --no-ignore --watch $$OPAMSWITCH/_build/default/react_example/server --filter 'main.exe' -- \
		$$OPAMSWITCH/_build/default/react_example/server/main.exe

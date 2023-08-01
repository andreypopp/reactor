start:
	@watchexec --restart --no-ignore --watch $$OPAMSWITCH/_build/default/react/react_example/server --filter 'main.exe' -- \
		$$OPAMSWITCH/_build/default/react/react_example/server/main.exe

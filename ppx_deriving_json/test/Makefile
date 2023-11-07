update-test: ppx_deriving_json_native.t ppx_deriving_json_browser.t

ppx_deriving_json_browser.t: example.ml
	bash ./gen_test.sh ../native/ppx_deriving_json_native_test.exe > $@

ppx_deriving_json_native.t: example.ml
	bash ./gen_test.sh ../browser/ppx_deriving_json_browser_test.exe > $@

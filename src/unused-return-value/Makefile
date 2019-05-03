
build:
	bapbuild -pkg bap-primus unused_return_value.plugin
	bapbundle update -desc "Detects unused return values" unused_return_value.plugin

install:
	bapbundle install unused_return_value.plugin

clean:
	bapbuild -clean

uninstall:
	bapbundle remove unused_return_value

reinstall:
	make uninstall
	make install

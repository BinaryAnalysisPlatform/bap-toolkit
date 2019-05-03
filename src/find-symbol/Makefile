
build:
	bapbuild find_symbol.plugin
	bapbundle update -desc "Find symbols that satisfy to certain criteria" find_symbol.plugin

install:
	bapbundle install find_symbol

uninstall:
	bapbundle remove find_symbol

reinstall:
	make uninstall
	make install

clean:
	bapbuild -clean


build:
	bapbuild -pkg bap-primus -pkg bap-taint null_ptr_deref.plugin
	bapbundle update -desc "detects null pointer dereference (CWE-476)" null_ptr_deref.plugin

install:
	bapbundle install null_ptr_deref

uninstall:
	bapbundle remove null_ptr_deref

reinstall:
	make uninstall
	make install

clean:
	bapbuild -clean

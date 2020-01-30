
build: clean
	sh build.sh build $(TARGET)

install:
	sh build.sh install $(TARGET)

test:
	make -C tests

clean:
	sh build.sh clean $(TARGET)

full: build install

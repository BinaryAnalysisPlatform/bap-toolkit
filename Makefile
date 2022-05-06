all: build install

install:
	sh build.sh install $(TARGET)

build: clean
	sh build.sh build $(TARGET)

test:
	make -C tests

clean:
	sh build.sh clean $(TARGET)


.PHONY: all install build clean test

PREFIX ?= /usr

.PHONY: build clean install uninstall

build:
	alr build --validation

clean:
	alr clean
	rm -rf build

install:
	install build/bin/evdev-ada $(PREFIX)/bin/

uninstall:
	rm $(PREFIX)/bin/evdev-ada

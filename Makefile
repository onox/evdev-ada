.PHONY: clean

all:
	gprbuild -P main.gpr
	./main /dev/input/event19
clean:
	gprclean -P main.gpr
	rmdir -p build/obj

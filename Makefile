.PHONY: clean

all:
	gprbuild -P main.gpr
	./main /dev/input/event18 -q
clean:
	gprclean -P main.gpr
	rmdir -p build/obj

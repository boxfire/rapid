RTS_SOURCES = builtin.c gc.c object.c rts.c
RTS_HEADERS = gc.h object.h
RTS_BITCODE = $(RTS_SOURCES:.c=.bc)

build/runtime.bc: Makefile build $(RTS_BITCODE)
	llvm-link -o $@ $(RTS_BITCODE)

build:
	-mkdir -p $@

	#$(RTS_HEADERS) Makefile
%.bc: %.c
	clang -flto -g -O3 -c -Wall -Wpedantic -Werror -I . -I ../external/llvm-statepoint-utils/dist -std=c11 -o $@ $<

clean:
	rm -f *.bc
	rm -rf build/

.PHONY: clean

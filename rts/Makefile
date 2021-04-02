RTS_SOURCES = builtin.c gc.c getline.c object.c rts.c time.c ../external/llvm-statepoint-utils/build/statepoint.c
RTS_HEADERS = gc.h object.h
RTS_BITCODE = $(RTS_SOURCES:.c=.bc)

UNAME_S := $(shell uname -s)

CFLAGS = -D_POSIX_C_SOURCE=200809L

ifeq ($(UNAME_S),Linux)
	CFLAGS += -D_GNU_SOURCE
endif
ifeq ($(UNAME_S),Darwin)
	CFLAGS += -D_DARWIN_C_SOURCE
endif

CFLAGS += -I .
CFLAGS += -I /usr/local/include
CFLAGS += -I ../external/llvm-statepoint-utils/dist

all: build/runtime.bc build/platform.a

build/runtime.bc: Makefile build $(RTS_BITCODE)
	llvm-link -o $@ $(RTS_BITCODE)

build/platform.a: Makefile build arch.o
	llvm-ar rcsu $@ arch.o

build:
	-mkdir -p $@

%.bc: %.c $(RTS_HEADERS) Makefile
	clang -flto -g -O3 -c -Wall -Wpedantic -Werror -Wno-format $(CFLAGS) -std=c11 -o $@ $<

arch.o: arch_x86-64.s
	clang -c -o $@ $<

clean:
	rm -f *.bc
	rm -rf build/

.PHONY: all clean

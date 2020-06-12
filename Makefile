all: rapid rts

rapid:
	idris2 --build rapid-cg.ipkg
	idris2 --build rapid-fe.ipkg

rts: bdw-gc rts/rts.bc

rts/rts.bc: Makefile rts/rts.c
	clang -flto -c -Wall -Wpedantic -Werror -I /usr/local/include -std=c99 -o rts/rts.bc rts/rts.c

bdw-gc: external/bdwgc/.libs/libgc.a

external/bdwgc/.libs/libgc.a: Makefile
	( cd external/bdwgc && test -f Makefile.in || autoreconf -vif && automake --add-missing )
	( cd external/bdwgc && test -f Makefile || ./configure --disable-debug --disable-gcj-support --enable-large-config --enable-static --disable-shared )
	make -j`nproc` -C external/bdwgc
	test -f $@ && touch $@

clean:
	rm -rf build rts/rts.bc samples/build
	find tests -type d -name build -ls
	make -C external/bdwgc distclean

.PHONY: all bdw-gc clean rapid rts

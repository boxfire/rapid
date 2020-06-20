all: rapid rts

rapid:
	idris2 --build rapid-cg.ipkg
	idris2 --build rapid-fe.ipkg

cg:
	idris2 --build rapid-cg.ipkg

rts: rts/rts.bc

rts/rts.bc: Makefile rts/rts.c
	clang -flto -g -c -Wall -Wpedantic -Werror -I ./external/llvm-statepoint-utils/dist -std=c99 -o rts/rts.bc rts/rts.c

clean: clean-tests
	rm -rf build rts/rts.bc samples/build

clean-tests:
	rm -rf tests/chez/*/build
	rm -rf tests/chez/*/output
	rm -rf tests/chez/*/compile.log

check: test

test: rts
	./runtests.sh --good

.PHONY: all check clean clean-tests rapid rts test

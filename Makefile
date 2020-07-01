all: llvm-passes rapid rts

rapid:
	idris2 --build rapid-cg.ipkg
	idris2 --build rapidc.ipkg

cg:
	idris2 --build rapid-cg.ipkg

rts:
	make -j `nproc` -C rts
	cp -v rts/build/runtime.bc support/rapid/runtime.bc

llvm-passes:
	(cd llvm && test -f Makefile || cmake .)
	make -C llvm

clean: clean-tests
	make -C rts clean
	make -C llvm clean
	rm -rf build samples/build

clean-tests:
	rm -rf tests/chez/*/build
	rm -rf tests/chez/*/output
	rm -rf tests/chez/*/compile.log

check: test

test: rts test-llvm
	./runtests.sh --good

test-llvm:
	make -C llvm test

.PHONY: all check clean clean-tests rapid rts test

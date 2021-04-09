all: llvm-passes rapid rts

rapid:
	idris2 --build rapid-cg.ipkg
	idris2 --build rapidc.ipkg

cg:
	idris2 --build rapid-cg.ipkg

rts:
	$(MAKE) -C rts
	cp -v rts/build/runtime.bc support/rapid/runtime.bc
	cp -v rts/build/platform.a support/rapid/platform.a

llvm-passes:
	(cd llvm && test -f Makefile || cmake .)
	$(MAKE) -C llvm
	cp -v llvm/librapid.so support/rapid/librapid.so

clean: clean-tests
	$(MAKE) -C rts clean
	$(MAKE) -C llvm clean
	rm -rf build samples/build

clean-tests:
	rm -rf tests/chez/*/build
	rm -rf tests/chez/*/output
	rm -rf tests/chez/*/compile.log

check: test

test: rts test-llvm
	./runtests.sh --good

test-llvm:
	$(MAKE) -C llvm test

.PHONY: all check clean clean-tests rapid rts test

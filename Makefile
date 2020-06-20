all: rapid rts

rapid:
	idris2 --build rapid-cg.ipkg
	idris2 --build rapid-fe.ipkg

cg:
	idris2 --build rapid-cg.ipkg

rts:
	make -j `nproc` -C rts

clean: clean-tests
	make -C rts clean
	rm -rf build samples/build

clean-tests:
	rm -rf tests/chez/*/build
	rm -rf tests/chez/*/output
	rm -rf tests/chez/*/compile.log

check: test

test: rts
	./runtests.sh --good

.PHONY: all check clean clean-tests rapid rts test

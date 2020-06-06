all: rapid

rapid:
	idris2 --build rapid-cg.ipkg
	idris2 --build rapid-fe.ipkg

clean:
	rm -rf build samples/build

.PHONY: all clean rapid

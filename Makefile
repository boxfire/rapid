all: rapid

rapid:
	idris2 --build rapid-cg.ipkg
	idris2 --build rapid-fe.ipkg

linked: rts_entry.o tiny.o
	gcc -g -o $@ $^

tiny.o: tiny.cmm
	ghc -g -cpp -dcmm-lint -keep-s-file -c $< -o $@

%.o: %.c
	gcc -g -c -o $@ $^

clean:
	rm -f tiny.o rts_entry.o linked

clean-samples:
	rm -rf samples/*.sexp samples/*.ll samples/*.s samples/*.native* samples/build

.PHONY: all clean clean-samples rapid

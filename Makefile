all: rapid

rapid:
	idris2 -p contrib -o rapid Rapid.idr

linked: rts_entry.o tiny.o
	gcc -g -o $@ $^

tiny.o: tiny.cmm
	ghc -g -cpp -dcmm-lint -keep-s-file -c $< -o $@

%.o: %.c
	gcc -g -c -o $@ $^

clean:
	rm -f tiny.o rts_entry.o linked

.PHONY: all build clean rapid

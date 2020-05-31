#!/bin/sh

set -e
set -u

root=$PWD

fdir="$(dirname "$1")"
fbase="$(basename "$1")"

(cd "$fdir" && "$root/build/exec/rapid2-fe" "${fbase}")
./build/exec/rapid2-cg "${1}.sexp"
cat support.ll "${1}.sexp.output.ll" > "${1}.full.ll"
opt "${1}.full.ll" | llc -o "${1}.s"

clang -c -o rts/rts.o rts/rts.c
clang -g -o "${1}.native" "${1}.s" rts/rts.o -lgc

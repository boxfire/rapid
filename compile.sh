#!/bin/sh

set -e
set -u

root=$PWD

fdir="$(dirname "$1")"
fbase="$(basename "$1")"

opt="${2:-}"
optimize=
optimizeO1="-mem2reg -inline -dce"
optimizeO2="$optimizeO1 -functionattrs -ipsccp -sccp -simplifycfg -gvn -ipconstprop -constprop -constmerge -adce -die -dse -deadargelim -globaldce"
if [ "$opt" = "-O1" ]; then
  optimize="$optimizeO1"
fi
if [ "$opt" = "-O2" ]; then
  optimize="$optimizeO2"
fi

set -x
(cd "$fdir" && "$root/build/exec/rapid2-fe" "${fbase}")
./build/exec/rapid2-cg "${1}.sexp"
cat support.ll "${1}.sexp.output.ll" > "${1}.full.ll"
opt "${1}.full.ll" $optimize | tee "${1}.bc" | llc -o "${1}.s"

set +x

clang -flto -c -o rts/rts.bc rts/rts.c
clang -g -o "${1}.native" "${1}.s" rts/rts.bc -lgc

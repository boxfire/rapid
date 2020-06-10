#!/bin/sh

set -e
set -u

root=$PWD

fdir="$(dirname "$1")"
fbase="$(basename "$1")"

workdir="$fdir/build/rapid"
workfile="$workdir/$fbase"
mkdir -p "$workdir"

opt="${2:-}"
debug=
optimize=
optimizeO1="-mem2reg -inline -dce"
optimizeO2="$optimizeO1 -functionattrs -ipsccp -sccp -simplifycfg -gvn -ipconstprop -constprop -constmerge -adce -die -dse -deadargelim -globaldce"
if [ -z "$opt" ]; then
  debug="--debug"
fi
if [ "$opt" = "-O1" ]; then
  optimize="$optimizeO1 -O1"
fi
if [ "$opt" = "-O2" ]; then
  optimize="$optimizeO2 -O2"
fi

set -x
(cd "$fdir" && "$root/build/exec/rapid2-fe" "${fbase}")
./build/exec/rapid2-cg $debug "${workfile}.sexp"
cat support.ll "${workfile}.sexp.output.ll" > "${workfile}.full.ll"
opt "${workfile}.full.ll" $optimize | tee "${workfile}.bc" | llc -tailcallopt -o "${workfile}.s"

clang -flto -c -I /usr/local/include -o rts/rts.bc rts/rts.c
clang -c -o "${workfile}.o" "${workfile}.s"
clang -g -o "${workfile}.native" "${workfile}.o" rts/rts.bc -L /usr/local/lib -lgc

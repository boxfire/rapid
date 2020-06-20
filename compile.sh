#!/bin/bash

set -e
set -u

set -o pipefail

root=$PWD

fdir="$(dirname "$1")"
fbase="$(basename "$1")"

workdir="$fdir/build/rapid"
workfile="$workdir/${fbase/%.idr/}"
mkdir -p "$workdir"

opt="${2:-}"
tco="-tailcallopt"
debug=
optimize=
optimizeO1="-mem2reg -always-inline -sccp -dce -rewrite-statepoints-for-gc -inline"
optimizeO2="$optimizeO1 -functionattrs -ipsccp -sccp -simplifycfg -gvn -ipconstprop -constprop -constmerge -adce -die -dse -deadargelim -globaldce -argpromotion"
optimizeO3="$optimizeO2"
if [ -z "$opt" ]; then
  debug="--debug"
  optimize="-mem2reg -sccp -dce -rewrite-statepoints-for-gc"
fi
if [ "$opt" = "-O1" ]; then
  debug="--debug"
  optimize="$optimizeO1 -O1"
fi
if [ "$opt" = "-O2" ]; then
  optimize="$optimizeO2 -O2 -tailcallelim"
fi
if [ "$opt" = "-O3" ]; then
  optimize="$optimizeO3 -O3"
fi

set -x
(cd "$fdir" && "$root/build/exec/rapid2-fe" "${fbase}")
./build/exec/rapid2-cg $debug "${workfile}.sexp"
cat support.ll "${workfile}.sexp.output.ll" > "${workfile}.full.ll"
opt "${workfile}.full.ll" $optimize | tee "${workfile}.bc" | llc $tco -o "${workfile}.s"
echo $'\n.globl __LLVM_StackMaps' >> "${workfile}.s"

clang -g -c -o "${workfile}.o" "${workfile}.s"
clang -g -o "${workfile}.native" "${workfile}.o" rts/build/runtime.bc external/llvm-statepoint-utils/dist/llvm-statepoint-tablegen.a

#!/bin/bash

set -e
set -u

set -o pipefail

root=$PWD

rapid_lib="$(dirname "$0")/llvm"

fdir="$(dirname "$1")"
fbase="$(basename "$1")"
fnoext="${fbase/%.idr/}"

workdir="$fdir/build/exec/${fnoext}_rapid"
workfile="$workdir/${fnoext}"
mkdir -p "$workdir"

opt="${2:-}"
tco="-tailcallopt"
debug=
clangopt=
optimize=
optimizeO1="-mem2reg -sccp -dce -rewrite-statepoints-for-gc -inline"
optimizeO2="$optimizeO1 -functionattrs -ipsccp -sccp -simplifycfg -gvn -ipconstprop -constprop -constmerge -adce -die -dse -deadargelim -globaldce -argpromotion"
optimizeO3="$optimizeO2"
if [ -z "$opt" ]; then
  debug="--debug"
  optimize="-globaldce -mem2reg -constprop -constmerge -sccp -dce -inline -instcombine -rewrite-statepoints-for-gc"
  clangopt="-O0"
fi
if [ "$opt" = "-O1" ]; then
  debug="--debug"
  optimize="-mem2reg -constmerge -sccp -dce -globaldce -rewrite-statepoints-for-gc"
  clangopt="-O1"
fi
if [ "$opt" = "-O2" ]; then
  #optimize="-globaldce -mem2reg  -tailcallelim -simplifycfg -sccp -dce -rewrite-statepoints-for-gc -dse -die -constprop -constmerge -basicaa -memdep -sccp -dce -die -gvn -mergefunc -globaldce -dce -sccp"
  optimize="-globaldce -mem2reg -basicaa -memdep -gvn -sroa -functionattrs -sccp -dce -constprop -constmerge -dse -dce -die -adce -mergefunc -ipconstprop -ipsccp -dce -die -dse -deadargelim -argpromotion -O2 -rewrite-statepoints-for-gc"
  clangopt="-O2"
fi
if [ "$opt" = "-O3" ]; then
  optimize="-globaldce -mem2reg -basicaa -memdep -gvn -sroa -functionattrs -sccp -dce -constprop -constmerge -dse -dce -die -adce -mergefunc -ipconstprop -ipsccp -dce -die -dse -deadargelim -argpromotion -rewrite-statepoints-for-gc -always-inline -inline -O3"
  clangopt="-O3"
fi


if [ "$opt" = "-test1" ]; then
  debug="--debug"
  optimize="-mem2reg -constmerge -sccp -dce -globaldce -rewrite-statepoints-for-gc"
fi
if [ "$opt" = "-test2" ]; then
  debug="--debug"
  optimize="-mem2reg -constmerge -sccp -dce -globaldce -instcombine -rewrite-statepoints-for-gc"
fi
if [ "$opt" = "-test3" ]; then
  debug="--debug"
  optimize="-mem2reg -constmerge -constprop -sccp -dce -globaldce -basicaa -memdep -reassociate -mergefunc -instcombine -functionattrs -ipsccp -simplifycfg -gvn -ipconstprop -adce -die -deadargelim -die -dse -dce -argpromotion -rewrite-statepoints-for-gc -always-inline -inline -dse -die -dce"
fi
if [ "$opt" = "-test4" ]; then
  debug=""
  optimize="-O3 -mem2reg -constprop -constmerge -sccp -dce -globaldce -rewrite-statepoints-for-gc"
fi

clangdebug=
if [ -n "$debug" ]; then
  clangdebug="-g"
fi

set -x
(cd "$fdir" && "$root/build/exec/rapidc" --codegen vmcode-sexp -o "${fnoext}" "${fbase}")
./build/exec/rapid2-cg $debug "${workfile}.sexp"
#cat support.ll "${workfile}.sexp.output.ll" > "${workfile}.full.ll"
opt -load "${rapid_lib}/librapid.so" "${workfile}.sexp.output.ll" $optimize -rapid-lower | tee "${workfile}.bc" | llc $tco -o "${workfile}.s"
opt -S < "${workfile}.bc" > "${workfile}.opt.ll"
echo $'\n.globl __LLVM_StackMaps' >> "${workfile}.s"

clang $clangdebug -c -o "${workfile}.o" "${workfile}.s"
clang $clangdebug $clangopt -o "${workfile}.native" "${workfile}.o" rts/build/runtime.bc external/llvm-statepoint-utils/dist/llvm-statepoint-tablegen.a

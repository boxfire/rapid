#!/bin/sh

set -e
set -u

./build/exec/rapid "${1}"
cat support.ll "${1}.output.ll" | opt -mem2reg -inline -dce | llc -o "${1}.s"
#cat support.ll "${1}.output.ll" | opt -time-passes -mem2reg -inline -reassociate -globalopt -die -dse -constmerge -constprop -tailcallelim -licm -gvn -dce | llc -o "${1}.s"

clang -o "${1}.native" "${1}.s" rts/rts.c

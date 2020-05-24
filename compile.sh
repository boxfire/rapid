#!/bin/sh

set -e
set -u

./build/exec/rapid "${1}"
cat support.ll "${1}.output.ll" | llc -o "${1}.s"

clang -g -o "${1}.native" "${1}.s" rts/rts.c

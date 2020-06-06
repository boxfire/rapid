#!/bin/bash

tests=(chez001 chez002 chez003 chez004)

if [[ -n "$1" ]]; then
  read -r -a tests <<< "$@"
fi

for test in ${tests[*]}; do
  testdir=tests/chez/$test
  idr=$(echo ${testdir}/*.idr)
  if ./compile.sh "$idr" >& "${testdir}/compile.log"; then
    "$testdir/build/rapid/$(basename "$idr").native" > "${testdir}/output"
    if git diff --quiet --no-index -- "${testdir}/expected" "${testdir}/output"; then
      echo "OK: $test"
    else
      git --no-pager diff --exit-code --no-index -- "${testdir}/expected" "${testdir}/output"
    fi
  else
    echo "COMPILE ERROR: $test"
    cat "${testdir}/compile.log"
  fi
done

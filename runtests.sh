#!/bin/bash

rapidc=$PWD/build/exec/rapidc

tests=(chez001 chez002 chez003 chez004 chez005 chez006 chez007 chez008 chez009 chez010 chez011 chez012 chez013 chez014 chez014 chez015 chez016 chez017 chez018 reg001)

if [[ -n "$1" ]]; then
  if [[ "$1" = "--good" ]]; then
    tests=(
      chez001
      chez002
      chez003
      chez004
      chez005
      chez006
      chez007
      chez008
      chez009
      chez011
      chez012
      reg001
      rapid001
      rapid002
      rapid003
      rapid004
      rapid005
      rapid006
    )
  else
    read -r -a tests <<< "$@"
  fi
fi

for test in ${tests[*]}; do
  testdir=tests/chez/$test
  idr=$(echo ${testdir}/*.idr)
  if $rapidc -o "$test" "$idr" >& "${testdir}/compile.log"; then
    "./build/exec/$test" > "${testdir}/output"
    #"$testdir/build/rapid/$(basename "$idr" .idr).native" > "${testdir}/output"
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

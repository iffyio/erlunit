#!/usr/bin/env bash

if [[ -z "$1" ]]; then
  echo "USAGE $ erlunit module_name"
  exit 1
fi

MODULE="$1"


erlc `dirname ${BASH_SOURCE}`/src/erlunit.erl

echo "compiling tests for $MODULE" 

erlc ${MODULE}{"",_test}.erl && {
  erl -noshell -run erlunit run_tests ${MODULE}_test -run init stop
}


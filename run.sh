#!/usr/bin/env bash

if [[ -z "$1" ]]; then
  echo "USAGE $ run module_name"
  exit 1
fi

MODULE="$1"


erlc `dirname ${BASH_SOURCE}`/src/erlunit.erl

#echo "compiling tests for $MODULE" 

erlc ${MODULE}{"",_tests}.erl && {
  erl -noshell -run erlunit run_tests ${MODULE}_tests -run init stop
}
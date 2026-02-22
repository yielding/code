#!/usr/bin/env bash

SLEEP=1
COUNT=1
while [ 1 ] ; do
  echo "repeat($COUNT th): " $@
  $@
  sleep $SLEEP
  let COUNT++
done

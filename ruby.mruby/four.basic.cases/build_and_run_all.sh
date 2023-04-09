#!/bin/bash

rm step1 step2 step3 step4

echo "======== step1 ========"
gcc -o step1 step1.c -I ../include -I ../src ../mrblib/mrblib.o ../lib/libmruby.a ../lib/libmruby_core.a -lm
./step1

echo "======== step2 ========"
gcc -o step2 step2.c -I ../include -I ../src ../mrblib/mrblib.o ../lib/libmruby.a ../lib/libmruby_core.a -lm
./step2

echo "======== step3 ========"
gcc -o step3 step3.c -I ../include -I ../src ../mrblib/mrblib.o ../lib/libmruby.a ../lib/libmruby_core.a -lm
./step3

echo "======== step4 ========"
gcc -o step4 step4.c -I ../include -I ../src ../mrblib/mrblib.o ../lib/libmruby.a ../lib/libmruby_core.a -lm
./step4



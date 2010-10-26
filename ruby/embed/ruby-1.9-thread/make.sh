#!/bin/sh
# builds and runs this example

ruby -v # just for reference

# build the makefile
ruby extconf.rb
sed 's/-shared\>//g; s/-O[0-9]\?\>//g' -i Makefile # no shared object, no optim
make clean

# build the C program
make

# run the C program
./main.so ||

# run the debugger upon failure
( test -e core* && gdb -c core* ./main.so )

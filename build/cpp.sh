#!/bin/sh
g++-12 -std=c++2a -o out $1
./out
rm out


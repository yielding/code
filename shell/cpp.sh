#!/bin/sh
g++-14 -std=c++2c -o out $1
./out
rm out

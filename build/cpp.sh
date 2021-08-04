#!/bin/sh
xcrun clang++ -o out $1
./out
rm out


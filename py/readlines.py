#!/usr/bin/env python

f = open("readlines.py", "r")

lines: list[str] = f.readlines()

line_no: int = 1

for line in lines:
    if line.startswith("for"):
        print("%d: %s" % (line_no, line))

    line_no += 1

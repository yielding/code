#!/usr/bin/env python

f = open("readlines.py", "r")

lines = f.readlines()

line_no = 1
for line in lines:
    if line.startswith("for"):
        print "%d: %s" % (line_no, line)

    line_no = line_no + 1

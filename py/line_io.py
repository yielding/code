#!/usr/bin/env python

h = {}
h[1] = 10
h[2] = 20

f = open("b.bin", "w")
for k,v in h.items():
    f.write("{0}, {1}\n".format(k, v))
f.close

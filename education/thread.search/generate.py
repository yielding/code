#!/usr/bin/env python

h = open("data.bin", "wb")

for i in xrange(1024*4):
  h.write("\x00")

h.seek(1)
h.write("monday")
h.seek(2048)
h.write("monday")
h.close()

#!/usr/bin/env python

h = open("data.bin", "wb")


h.seek(1)
h.write("monday")
h.seek(1024*1024*1024-100)
h.write("monday")
h.close()

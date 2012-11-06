#!/usr/bin/env python

import cPickle

h = {}

h[0xFFFFFF00L] = 0x12345678
h[0xFFFFFF01L] = 0x22345678
h[0xFFFFFF02L] = 0x32345678

f = open("pickled2.bin", "wb")
cPickle.dump(h, f, cPickle.HIGHEST_PROTOCOL)
f.close()

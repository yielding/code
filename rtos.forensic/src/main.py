#!/usr/bin/env python
# -*- coding: utf-8 -*-

from vfs.datastore import *

ds = DataStore()

path = "/Users/yielding/code/rtos.forensic/README.md"
f    = open(path)
ds.build_filesystem(f)

f.close()

print "ok"

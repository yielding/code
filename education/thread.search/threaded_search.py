#!/usr/bin/env python

import threading
import time
import search
import os

class SearchThread(threading.Thread):
  def __init__(self, path, beg, end, key):
    threading.Thread.__init__(self)
    self.path = path
    self.beg  = beg
    self.end  = end
    self.key  = key
    self.offset = []

  def run(self):
    s = open(self.path, "rb")
    self.offset = search.search_file(s, self.beg, self.end, self.key)
    s.close()

p0 = "data0.bin"
p1 = "data1.bin"
size = os.path.getsize(p0)
th1 = SearchThread(p0, 0, size-1, "monday")
th2 = SearchThread(p1, 0, size-1, "monday")

th1.start()
th2.start()
threads = [th1, th2]

[t.join() for t in threads]

print [t.offset for t in threads]

#!/usr/bin/env python

def create_test_file(name, key, size, locations):
  h = open(name, "wb")
  h.seek(size-1)
  h.write("\0")

  for loc in locations:
    h.seek(loc)
    h.write(key)

  h.close()

k = 1024
m = k * 1024
g = m * 1024

create_test_file("data0.bin", "monday", g, [1, g-10])
create_test_file("data1.bin", "monday", g, [1, g-10])

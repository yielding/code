#!/usr/bin/env python
# encoding: utf-8

import os

def search_block(block, key):
  pos_list = []

  pos = block.find(key)
  while pos >= 0:
    pos_list.append(pos)
    pos = block.find(key, pos+1)

  return pos_list

def search_file(s, beg, end, key):
  size = end - beg + 1
  blks = (size + 1023) / 1024
  print "size: %d, block count: %d " % (size,  blks)

  offsets = []
  for i in range(blks):
    s.seek(beg + i*1024)
    block = s.read(1024 + len(key)-1)
    roffsets = search_block(block, key)
    offsets += map(lambda x: x + i*1024 + beg, roffsets)

  return offsets

if __name__ == "__main__":
  path = "data.bin"
  size = os.path.getsize(path)

  #block = "leech\x00\x00leech\x00leech"
  # 한글
  h = open(path, "rb")
  print search_file(h, 0, size/2-1, "monday")
  print search_file(h, size/2, size-1, "monday")
  h.close()

#!/usr/bin/env python
#coding: utf-8

def translate(s):
  res = map(lambda x: '.' if ord(x) % 2 == 0 else x, s)
  return "".join(res)


s = "leech babo"
arr = [c for c in s]
print arr

print translate(arr)

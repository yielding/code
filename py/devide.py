#!/usr/bin/env python

def devide(a, b):
  q = a // b
  r = a - q*b
  return (q, r)

print devide(10, 3)

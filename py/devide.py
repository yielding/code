#!/usr/bin/env python

def devide(a, b):
  """docstring for devide"""
  q = a // b
  r = a - q*b
  return (q, r)

print devide(10, 2)

#!/usr/bin/env python

def fib(n):
  if n == 0 or n == 1:
    return n
  else:
    return fib(n-1) + fib(n-2)

for i in range(36):
  print("n=%d => %d" % (i, fib(i)))


#!/usr/bin/env python

import pdb

def fibonacci(max) -> Generator[int, None, None]:
    """docstring for fibonacci"""
    s: int = 1
    t: int = 1
    while s < max:
        yield s
        w = s + t
        s = t
        t = w

for n in fibonacci(10000):
  print(n)

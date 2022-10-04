#!/usr/bin/env python

def devide(a: int, b: int) -> tuple[int, int]:
  q: int = a // b
  r: int = a - q*b

  return (q, r)

print(devide(10, 3))

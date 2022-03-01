#!/usr/bin/env python

a = sum(range(101)) ** 2
b = sum(map(lambda x:x*x, range(101)))

print(abs(a - b))

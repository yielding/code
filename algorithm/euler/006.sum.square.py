#!/usr/bin/env python

a = sum(xrange(101)) ** 2
b = sum(map(lambda x:x*x, xrange(101)))

print(abs(a - b))

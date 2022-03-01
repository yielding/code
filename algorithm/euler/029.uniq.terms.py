#!/usr/bin/env python

from itertools import product

r = range(2,101)
print(len(set([a**b for a, b in product(r, r)])))

#!/usr/bin/env python

from itertools import permutations

n = 4
cols = range(n)
for v in permutations(cols):
    l1 = len(set(v[i] + i for i in cols))
    l2 = len(set(v[i] - i for i in cols))

    print set(v[i] + i for i in cols)
    print set(v[i] - i for i in cols)

    if (l1 == l2 == n):
        print v

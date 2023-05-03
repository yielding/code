#!/usr/bin/env python

from functools import partial
from functools import reduce
import operator

def each_cons_iter(listin, n): 
    i = 0
    while (i<len(listin)-n+1):
        yield listin[i:i+n]
        i += 1

each_cons = partial(each_cons_iter, n = 5)

def product(l):
  return reduce(operator.mul, l)

a: list[int] = [i for i in range(1, 10)]
b = [i for i in each_cons(a)]

print(b)
print(map(product, b))
print(max(map(product, b)))

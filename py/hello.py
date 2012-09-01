#!/usr/bin/env python

from functools import partial

def power(x, y):
    return x ** y
    
square = partial(power, y = 2)

# or 
print sum(map(square, 
              filter(lambda x:x%2==1, range(10))))

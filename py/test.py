#!/usr/bin/env python

import string

square = lambda x: x*x
is_odd = lambda x: x%2 == 1

v = sum(map(square, 
            filter(is_odd, range(10))))
print v

# or 
print sum(map(lambda x:x*x, 
              filter(lambda x:x%2==1, range(10))))


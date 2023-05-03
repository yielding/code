#!/usr/bin/env python

from functools import partial

# case 1
total = 0
for i in range(10):
    if i < 5:
        total += i * i

print(total)

# case 2
print(sum(map(lambda x:x*x,
              filter(lambda x:x<5, range(10)))))

# case 3
def power(x, y):
    return x ** y

# square = partial(power, 2) # x = 2
# cube   = partial(power, 3) # x = 3
square = partial(power, y=2)
cube   = partial(power, y=3)
lt_5   = lambda x: x < 5

print(sum(map(square, filter(lt_5, range(10)))))
print(sum(map(cube,   filter(lt_5, range(10)))))
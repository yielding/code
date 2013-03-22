from functools import partial
import operator

def each_cons_iter(l, n):
    i = 0
    while i < len(l) - n + 1:
        yield l[i:i+n]
        i += 1

each_cons = partial(each_cons_iter, n = 5)

a   = [i for i in range(1, 10)]
print [i for i in each_cons(a)]
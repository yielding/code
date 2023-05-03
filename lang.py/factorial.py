#!/usr/bin/env python

def factorial(n) -> int:
    """docstring for factorial"""
    result = 1
    for i in range(1, n+1):
        result *= i

    return result

if __name__ == '__main__':
    f20  = factorial( 20)
    f200 = factorial(200)
    print("fact(20)  : %d, type: %s" % (f20, type(f20)))
    print("fact(200) : %d, type: %s" % (f200, type(f200)))

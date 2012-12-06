#!/usr/bin/env ruby19

# [1; (2)]
# [2; 1, 2, 1, 1, 4, 1, 1, 6, 1]

a = 1 + Rational(1,2)
b = 1 + Rational(1, 2 + Rational(1, 2))
c = 1 + Rational(1, 2 + Rational(1, 2 + Rational(1, 2)))

p a, b, c


#!/usr/bin/env ruby19

# [n2, n2 - n + 1, n2 -2n + 2, n2 - 3n + 3]
p 1 + 3.step(1001, 2).map { |n| 4*n*n - 6*n + 6 }.reduce(:+)

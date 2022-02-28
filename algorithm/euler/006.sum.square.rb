#!/usr/bin/env ruby

a1 = (1..100).map { |e| e ** 2 }.reduce(:+)
a2 = (1..100).reduce(:+) ** 2

p (a2 - a1).abs

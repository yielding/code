#!/usr/bin/env ruby
# encoding: utf-8

r = rand(100)
a = [*1..100]
m = a - [r]

p a.reduce(:+) - m.reduce(:+)


#!/usr/bin/env ruby19 -wKU

a = [*1..100]

a1 = a.map { |e| e ** 2 }.reduce(:+)
a2 = a.reduce(:+) ** 2

p a2 - a1

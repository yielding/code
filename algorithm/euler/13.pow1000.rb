#!/usr/bin/env ruby -wKU

v = 2**1000

res = 0
while v / 10 > 0
  res += v % 10
  v = v / 10
end

p res + v

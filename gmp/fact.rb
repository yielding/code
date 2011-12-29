#!/usr/bin/env ruby -wKU

def fact n
  res = 1
  1.upto(n) { |i| res *= i }
  res
end

p fact 200000

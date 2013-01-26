#!/usr/bin/env ruby1.9

def fact n
  res = 1
  n.downto(1) { |i| res *= i }
  res
end

fact 200000

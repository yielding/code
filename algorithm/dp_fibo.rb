#!/usr/bin/env ruby

def fibo n
  return n if n <= 1

  cache    = [0] * (n+1)
  cache[1] = 1

  2.upto(n) { |i| cache[i] = cache[i-1] + cache[i-2] }
  cache[n]
end

1.upto(1000) { |i| p fibo i }
p (1..2000).reduce(0) { |r, n| r + fibo(n) }

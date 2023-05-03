#!/usr/bin/env ruby

def fibo n
  return n if n <= 1

  cache = [0] * (n+1)
  cache[1] = 1

  2.upto(n) { |i| cache[i] = cache[i-1] + cache[i-2] }
  cache[n]
end

def fibo2 n
  return n if n <= 1
  return fibo2(n-1) + fibo2(n-2)
end


for i in 1..10000
  result = fibo(i)
  puts result

  break if result > 1000000000
end

#1.upto(1000) { |i| p fibo i }
#p (1..2000).reduce(0) { |r, n| r + fibo(n) }

#!/usr/bin/env ruby

def fibo n
  n < 2 ? n : fibo(n-1) + fibo(n-2)
end

p fibo 40

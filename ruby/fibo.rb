#!/usr/bin/env ruby19

def fibo n
  return 1 if n == 1 or n == 2

  fibo(n-1) + fibo(n-2)
end

p fibo 40

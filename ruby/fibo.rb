#!/usr/bin/env ruby
# encoding: utf-8

def fibo n
  n < 2 ? n : fibo(n-1) + fibo(n-2)
end

p fibo 40

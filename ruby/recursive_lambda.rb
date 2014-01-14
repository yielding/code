#!/usr/bin/env ruby
# encoding: utf-8

def fact n
  f = -> r { r == 0 ? 1 : r * f.call(r-1) }
  f.call(n)
end

p fact 3

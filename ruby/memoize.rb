#!/usr/bin/env ruby
# encoding: utf-8

module Memoize
  def memoize name
    meth  = method(name)
    cache = Hash.new { |cache, args| cache[args] = meth.call(*args) }
    ( class << self; self; end ).class_eval do
        define_method(name) { |*args| cache[args] }
    end
    cache
  end
end

class Series
  def fibo n
    return 1 if n == 1 or n == 2
    return fibo(n-1) + fibo(n-2)
  end
end

s = Series.new

Series.extend(Memoize)
Series.memoize(:fibo)

puts s.fibo(100)

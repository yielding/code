#!/usr/bin/env ruby
# encoding: utf-8

def Integer.all
  Enumerator.new do |yielder, n: 0|
    loop { yielder.yield(n += 1) }
  end.lazy
end

def palindrome?(n)
  n = n.to_s
  n == n.reverse
end

p Integer.all.first(10),
  Integer.all
         .select { |i| (i % 3).zero?  }
         .select { |i| palindrome?(i) }
         .first(10)

#!/usr/bin/env ruby

require_relative "numeric_ext"

beg = 600851475143

max = Math.sqrt(beg).floor # => 775146

2.upto(max) { |n| 
  r = beg % n
  d = beg / n
  if r == 0
    printf("%03d, %d, %d\n", n, d, r)
    puts "#{n} is prime" if n.is_prime?
  end
}

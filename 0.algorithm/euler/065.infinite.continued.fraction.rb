#!/usr/bin/env ruby

=begin rdoc
  Term  Numerator  Multiplier  Formula
 ---------------------------------------------
   0        1         
   1        2        2          2 * no0
   2        3        1          1 * no1 + no0
   3        8        2          2 * no2 + no1
   4       11        1          1 * no3 + no2
   5       19        1          1 * no4 + no3
   6       87        4          4 * no5 + no4
   7      106        1          1 * no6 + no5
   8      193        1          1 * no7 + no6
   9     1264        6          6 * no8 + no7
  10     1257        1          1 * no9 + no8
=end

class Numeric
  def digits
    self.to_s.scan(/./).map {|e| e.to_i }
  end
end

module Enumerable
  def sum
    self.reduce(:+)
  end
end

def e n
  return n + 1 if n < 2
  cache = [0] * 1000
  cache[0], cache[1] = 1, 2

  2.upto(n) { |i| 
    multiplier = (i % 3).zero? ? 2 * (i / 3) : 1
    cache[i] = multiplier * cache[i-1] + cache[i-2]
  }

  return cache[n]
end

p e(100).digits.sum

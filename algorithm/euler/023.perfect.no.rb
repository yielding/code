#!/usr/bin/env ruby

require "numeric_ext"

class Numeric
  def perfect?
    self.proper_divisors.reduce(:+).eql?(self)
  end

  def abundant?
    self.proper_divisors.reduce(:+) > self
  end
end

a = 2.upto(28123).select { |n| n.abundant? }

# in this case "product" is too slow to use
# res = a.product(a).select { |e| e.reduce(:+) <= 28123 }

res = []
a.each { |i|  a.each { |j| res << i + j if i + j <= 28123 } }

p ([*1..28123] - res).reduce(:+)

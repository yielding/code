#!/usr/bin/env ruby19

require_relative "divisor"

class Numeric
  def sum_of_proper_divisors
    divisors2[0..-2].reduce(:+)
  end

  def amicable?
    v1 = self.sum_of_proper_divisors
    return false if v1 <= 1

    v2 = v1.sum_of_proper_divisors
    (v1 != v2) and self.eql?(v2)
  end
end

p 2.upto(10000).select { |n| n.amicable? } \
               .reduce(:+)


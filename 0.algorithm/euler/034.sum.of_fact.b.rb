#!/usr/bin/env ruby

require "numeric_ext"

class Numeric
  def fact_sum
    val = self
    sum = 0
    while val > 0
      val, r = val.divmod(10)
      sum += r.factorial
    end

    sum 
  end
end

p (10..2540160).select { |n| n.fact_sum == n }
               .reduce(:+)

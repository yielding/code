#!/usr/bin/env ruby

require_relative "numeric_ext"

class Numeric
  def digit_count
    val = self
    count = 0
    while val > 0
      count += 1
      val /= 10
    end

    count
  end

  def rshifts
    val,res = self, []
    while val > 0
      res << val
      val /= 10
    end

    res.shift if res.size > 1
    res
  end

  def lshifts
    val   = self
    count = self.digit_count
    res   = []
    while count > 1
      val = val % 10 ** (count - 1)
      res << val
      count -= 1
    end

    res
  end
end

r = (10..1000000)
p r.select { |e| e.is_prime? }
   .map    { |e| ([e] + e.rshifts + e.lshifts).uniq }
   .select { |e| e.all? { |n| n.is_prime? } }
   .inject(0) { |s, e| s + e[0] }
            
=begin
res = []
a = (10..1_000_000).select { |e| e.is_prime? }
a.each { |e|
  s = (e.rshifts + e.lshifts).uniq
  res << e if s.all? { |n| n.is_prime? }
}

p res
p res.sum
=end

#!/usr/bin/env ruby19

require "mathn"

class Numeric
  def prime?
   v = Math.sqrt(self).floor
   2.upto(v) { |n| return false if self % n == 0 }
   true
  end
end

a = (2..2000000).select { |n| n.prime? }
p a.reduce(:+)

#!/usr/bin/env ruby19

require_relative "divisor"

class Numeric
  def triangular_no
    return (self / 2) * (self + 1) if self % 2 == 0
    return self * (self + 1) / 2
  end
end

i = 2
loop do
  v = i.triangular_no
  count = v.divisors2.size
  p "#{v}: #{count}"
  if count >= 500
    p v
    exit
  end

  i += 1
end

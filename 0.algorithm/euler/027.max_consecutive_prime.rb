#!/usr/bin/env ruby

require "prime"

class Numeric
  def prime?
    return false if self < 2 
    return true  if self < 4 
    return false if self % 2 == 0
    return true  if self < 9 
    return false if self % 3 == 0

    r = Math.sqrt(self).floor
    f = 5 
    while f <= r
      return false if self % f == 0
      return false if self % (f + 2) == 0
      f += 6
    end 
    true
  end
end

max_sequence = 0
max_a, max_b = 0, 0

for a in -999..999
  for b in -999..999
    n  = 0
    n += 1 while (n*n + a*n + b).prime?
    if n > max_sequence
      max_sequence = n 
      max_a = a
      max_b = b
    end
  end
end

p max_a * max_b

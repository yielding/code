#!/usr/bin/env ruby

class Numeric
  def is_prime?
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

p [*2..2000000].select { |n| n.is_prime? }.sum

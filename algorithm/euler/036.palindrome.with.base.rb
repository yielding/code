#!/usr/bin/env ruby19

class Numeric
  def to_bin
    val = self
    res, count = 0, 0
    while val > 0
      val, r = val.divmod(2)
      res   += (10 ** count * r)
      count += 1
    end
    
    res
  end

  def is_palindrome? base
    val = self
    reversed = 0
    while val > 0
      reversed = base * reversed + val % base;
      val /= base
    end

    reversed == self
  end
end

=begin rdoc
a = [*1..1_000_000]
p a.select { |e| e.is_palindrome? }
   .select { |e| e.to_bin.is_palindrome? }
   .reduce(:+)
=end

a = [*1..1_000_000]
p a.select { |e| e.is_palindrome?(2) and e.is_palindrome?(10) }
   .reduce(:+)

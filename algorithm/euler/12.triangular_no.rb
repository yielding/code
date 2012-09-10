#!/usr/bin/env ruby19

require "prime"

class Numeric
  def divisors
    # 1. the below algorithm is too simplistic to apply to a big number
    (1..self).select { |n| (self / n) * n == self }
  end

  def divisors
    primes, powers = self.prime_division.transpose
    exponents = powers.map { |i| (0..i).to_a }
    result = exponents.shift.product(*exponents).map { |powers|
      primes.zip(powers).map { |prime, power| prime ** power }.reduce(:*)
    }

    result.sort.map { |div| [div, self / div] }
  end

  def triangular_no
    return self / 2 * (self + 1) if self % 2 == 0
    return self * (self + 1) / 2
  end
end

i = 2
while i > 0
  v = i.triangular_no
  count = v.divisor_count
  p "#{v}: #{count}"
  if count >= 500
    p v
    exit
  end

  i += 1
end

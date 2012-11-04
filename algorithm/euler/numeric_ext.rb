require "prime"

class Numeric
  def naive_divisors
    # 1. the below algorithm is too simplistic to apply to a big number
    (1..self).select { |n| (self / n) * n == self }
  end

  def divisors
    primes, powers = self.prime_division.transpose
    exponents = powers.map { |i| (0..i).to_a }
    result    = exponents.shift.product(*exponents).map { |powers|
      primes.zip(powers).map { |prime, power| prime ** power }.reduce(:*)
    }

    result.sort.map { |div| div }
  end

  def proper_divisors
    return [] if self == 1
    divisors - [self]  
  end

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

  def factorial
    self == 0 ? 1 : 1.upto(self).reduce(:*)
  end

  def is_palindrome?
    s = self.to_s
    s == s.reverse
  end

  def is_pandigital?
    pandigital?
  end

  def pandigital?
    digits, count = 0, 0
    val = self
    while val > 0
      tmp = digits
      digits |= 1 << (val % 10 - 1)

      return false if tmp.eql?(digits)

      count += 1
      val /= 10
    end

    digits == (1 << count) - 1
  end

  def concat no
    return self * 10 if no == 0

    tmp, pow = no, 1
    while tmp > 0
      pow *= 10
      tmp /= 10
    end

    self * pow + no
  end

  def digit_count
    v, count = self, 0

    while v > 0
      v /= 10
      count += 1
    end

    count
  end
end

class Array
  def sum
    self.reduce(:+)
  end
end

if __FILE__ == $PROGRAM_NAME
  p 28.divisors
  p 28.proper_divisors
  p 131.is_palindrome?
  p [28, 29].map { |e| p e.is_prime? }
  p 978654321.is_pandigital?
  p 78654321.is_pandigital?
  p 99999.digit_count
end

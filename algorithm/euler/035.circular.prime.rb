#!/usr/bin/env ruby

require "set"
require "numeric_ext"

class Numeric
  def digit_count
    val, count = self, 0
    while val > 0
      count += 1
      val /= 10
    end

    count
  end

  def rotations
    val = self
    res = []
    multiplier = 10 ** (self.digit_count - 1)
    digit_count.times {
      val, r = val.divmod(10)
      val    = r * multiplier + val 
      res << val
    }

    res - [self]
  end

end

def eratosthenes_sieve upper_limit
  sieve_bound = (upper_limit - 1 ) / 2 
  upper_sqrt  = Integer((Math.sqrt(upper_limit) - 1) / 2)

  prime_bits = [true] * (sieve_bound + 1)
  1.upto(upper_sqrt) { |i| 
    if prime_bits[i]
      (i*2 * (i + 1)).step(sieve_bound, 2*i + 1) { |j| 
        prime_bits[j] = false
      }   
    end 
  }

  numbers = [2] 

  1.upto(sieve_bound) { |i| numbers << (2*i + 1) if prime_bits[i] }

  numbers
end


primes    = eratosthenes_sieve(1_000_000)
prime_set = primes.to_set

p primes.select { |n| n.rotations.to_set.subset?(prime_set) }.size
  

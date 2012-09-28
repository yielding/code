#!/usr/bin/env ruby19

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

p eratosthenes_sieve(2000000).reduce(:+)

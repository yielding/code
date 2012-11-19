#!/usr/bin/env ruby19

require_relative "sieve"

class Numeric
  def twice_square?
    t = Math.sqrt(self/2.0)
    return t == Integer(t)
  end
end

from = 2
to   = 10000
primes = eratosthenes_sieve(to)
odds   = (from..to).select { |n| n.odd? }
composite = odds - primes

found = false

composite.each { |c|
  break if found
  found = true

  primes.each { |prime|  
    break if c <= prime
    begin found = false; break; end if (c - prime).twice_square?
  }

  p c if found
}

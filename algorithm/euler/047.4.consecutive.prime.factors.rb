#!/usr/bin/env ruby

require_relative "numeric_ext"
require "pp"

count = 0
first = 0
10000.upto(200000) { |n|
  primes, powers = n.prime_division.transpose
  unless primes.size == 4
    count = 0
    next
  end

  first = n
  count = count + 1
  if count == 4
    p first - 3
    break
  end
}

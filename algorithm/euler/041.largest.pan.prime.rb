#!/usr/bin/env ruby

require_relative "numeric_ext"
require_relative "sieve"

res = eratosthenes_sieve(7654321)
p res.select {|e| e.is_pandigital? }.max

#!/usr/bin/env ruby

require "numeric_ext"

a = (2..2000000).select { |n| n.is_prime? }
p a.reduce(:+)

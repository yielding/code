#!/usr/bin/env ruby

require "numeric_ext"

p (2..2000000).select { |n| n.is_prime? }
              .reduce(:+)

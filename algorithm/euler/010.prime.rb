#!/usr/bin/env ruby

require_relative "numeric_ext"

p (2..2000000).select { |n| n.is_prime? }
              .reduce(:+)

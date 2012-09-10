#!/usr/bin/env ruby19

require "prime"

a = (2..2000000).select { |n| n.prime? }
p a.reduce(:+)

#!/usr/bin/env ruby19

require_relative "numeric_ext"
require "pp"

h = Hash.new { |h, k| h[k] = [] }

# for a in 1..500
#   b1, e1 = 1, 1000 - a
#   for b in b1..e1
#     b2, e2 = 1, 1000 - a - b
#     for c in b2..e2
#       p = a + b + c
#       next if p >= 1000
#       if a*a + b*b == c*c
#         pp [a, b, c] 
#         h[p] << [a, b, c]
#       end
#     end
#   end
# end

=begin rdoc
 a*a + b*b = c*c -- (1)
 a + b + c = p   -- (2)
 b = (p*p - 2ap)/(2p - 2a)
=end

pp h

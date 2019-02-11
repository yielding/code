#!/usr/bin/env ruby

# 
# fine abc where (1) a + b + c = 1000
#                (2) a^2 + b^2 = c^2
#
# find (1, 2, 3) 순서쌍
#

# for a in (1...500)
#   for b in (1...500)
#     c = 1000 - a - b
#     if a*a + b*b == c*c
#       p [a, b, c]
#       p a * b * c 
#       exit
#     end
#   end
# end

require "mathn"

def find_triplet sum 
  m = 0
  (0..sum).each { |n|
    delta = n*n + 2*sum
    m1    = (-n + Math.sqrt(delta)) / 2
    m2    = (-n - Math.sqrt(delta)) / 2
    if m1.integer? && m1 > n
      m = m1
    elsif m2.integer? && m2 > n
      m = m2
    end

    return [2*m*n, m*m - n*n, m*m + n*n] unless m.zero?
  }
end

a, b, c = find_triplet(1000)

p [a, b, c]
p [a, b, c].reduce(:*)

#!/usr/bin/env ruby19

require_relative "numeric_ext"
require "pp"

h = Hash.new { |h, k| h[k] = [] }
pp h

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
 a * a + b * b = c * c -- (1)
 a + b + c = p         -- (2)
 b = (p * p - 2ap)/(2p - 2a)
=end

tmx, smx = 0, 0
for s in 1..1000
  t = 0
  mlimit = Integer(Math.sqrt(s/2))
  for m in 2..mlimit
    if (s / 2) % m == 0
      k = m.even? ? m + 1 : m + 2
      while k < 2*m and k <= s / (2*m)
        t += 1 if s / (2 * m) % k == 0 and k.gcd(m) == 1
        k += 2
      end
    end
  end

  if t > tmx
    tmx = t
    smx = s
  end
end

p smx

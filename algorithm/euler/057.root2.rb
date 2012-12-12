#!/usr/bin/env ruby19

require_relative "numeric_ext"
=begin rdoc
# [1; (2)]

a = 1 + Rational(1,2)
b = 1 + Rational(1, 2 + Rational(1, 2))
b = 1 + Rational(1, 1 + 1 + Rational(1, 2))
c = 1 + Rational(1, 2 + Rational(1, 2 + Rational(1, 2)))
p a, b, c
    
def sqrt(n)
  return n.zero? ? 1 + Rational(1, 1 + 1) : 1 + Rational(1, 1 + sqrt(n-1))
end

count = 0
0.upto(1000) { |e|
  res = sqrt(e)
  a = res.numerator.digit_count
  b = res.denominator.digit_count

  count += 1 if a > b
}
p count
=end


=begin rdoc
   3/2, 7/5, 17/12, 41/29 
=end

def root2 n
  count = 0
  no, deno = 3, 2
  n.times { 
    no, deno = no + 2 * deno, no + deno 
    count += 1 if no.digit_count > deno.digit_count
  }

  count
end

p root2(1000)

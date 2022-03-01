#!/usr/bin/env ruby

=begin
res = []
for a in 2..100
  for b in 2..100
    res << a ** b
  end
end
p res.uniq.size
=end

a = [*2..100]
p a.product(a).map { |a, b| a ** b }.uniq.size

#!/usr/bin/env ruby19

target    = 200
coin_size = [1, 2, 5, 10, 20, 50, 100, 200]
ways      = [1] + [0] * target

for i in 0...coin_size.size
  for j in coin_size[i]..target
     ways[j] += ways[j - coin_size[i]]
  end
end

p ways[target]

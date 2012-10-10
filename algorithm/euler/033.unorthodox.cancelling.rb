#!/usr/bin/env ruby19

a = [*10..99]
p a.product(a)
   .select { |a, b| a.quo(b) < 1 }
   .select { |a, b| a % 10 == b / 10 }
   .select { |a, b| b % 10 != 0 }
   .select { |a, b| a.quo(b) == (a/10).quo(b%10) }
   .map    { |a, b| a.quo(b) }
   .reduce(:*)

=begin rdoc
arr = []
for i in 10..99
  for j in 10..99
    a, b  = i % 10, j / 10
    next unless a == b

    nom, denom = i / 10, j % 10
    next if denom == 0
    next if i.quo(j) >= 1

    if nom.quo(denom) == i.quo(j)
      puts "#{i}, #{j}, #{i.quo(j)}" 
      arr << i.quo(j)
    end
  end
end

p arr.reduce(:*)
=end

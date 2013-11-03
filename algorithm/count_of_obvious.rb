#!/usr/bin/env ruby

def count_of_obvious n
  same = 0
  0.upto(n) { |x|
    y = 1.0 / x
    puts y * x 
    same += 1 if y * x - 1.0 == 0.0
  }

  same
end

puts count_of_obvious(50)

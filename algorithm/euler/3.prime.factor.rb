#!/usr/bin/env ruby -wKU

class Fixnum
  def prime?
    return false if self < 2
    2.upto(self/2) { |n| return false if self % n == 0 }
    true
  end
end

beg = 600851475143

max = Math.sqrt(beg).floor # => 775146

2.upto(max) { |n| 
  r = beg % n
  d = beg / n
  if r == 0
    printf("%03d, %d, %d\n", n, d, r)
    puts "#{n} is prime" if n.prime?
  end
}

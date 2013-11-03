#!/usr/bin/env ruby

class Float
  def equal? x
    diff = (self - x).abs
    return true if diff < 1e-10
    diff <= (1e-8 * [self.abs, x.abs].max)
  end
end

def count_of_obvious n
  same = 0
  1.upto(n) { |x|
    y = 1.0 / x
    puts y * x 
    same += 1 if y * x - 1.0 == 0.0
  }

  same
end

def count_of_obvious2 n
  same = 0
  1.upto(n) { |x|
    y = 1.0 / x
    puts y * x 
    same += 1 if (y * x).equal?(1.0)
  }

  same
end

puts count_of_obvious2(50)

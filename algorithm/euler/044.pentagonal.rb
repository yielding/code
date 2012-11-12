#!/usr/bin/env ruby19

require "pp"

class Numeric
  def is_pentagonal?
    v = (Math.sqrt(1 + 24*self) + 1.0) / 6.0
    return v == Integer(v)
  end
end

i = 1
loop do
  i += 1
  n = i * (3*i - 1) / 2
  (i-1).downto(1) { |j|
    m = j * (3*j - 1) / 2
    if (n - m).is_pentagonal? and (n + m).is_pentagonal?
      p "#{n}, #{m}"
      p n - m
      exit
    end
  }

end

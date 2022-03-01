#!/usr/bin/env ruby

require "numeric_ext"

class Numeric
  def hexa
    return self * (2 * self - 1)
  end

  def tri
    return self * (self + 1) / 2
  end
end

n = 144
while true
  val = n.hexa
  begin p val; break end if val.is_pentagonal?
  n += 1
end

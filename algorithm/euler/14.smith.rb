#!/usr/bin/env ruby19

class Numeric
  @@length = Hash.new(0)

  def smith
    n = self
    length = 1
    until n == 1
      n = n.modulo(2) == 0 ? n.div(2) : n*3 + 1
      if n < self and @@length[n] != 0
        @@length[self] = @@length[n] + length
        return @@length[self]
      end
      length += 1
    end

    @@length[self] = length
    @@length[self]
  end
end

t0 = Time.now

start, largest = 1, 0
for i in (start..1000000)
  sz = i.smith
  start, largest = i, sz if sz > largest
end

p Time.now - t0
p "start number: #{start}, count: #{largest}"

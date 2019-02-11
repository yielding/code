#!/usr/bin/env rub

require_relative "numeric_ext"

class Numeric
  def digits
    val = self
    res = []
    while val > 0
      val, r = val.divmod(10)
      res << r
    end
    res.reverse
  end

  def concat b
    tmp, pow = b, 1
    while tmp > 0
      pow *= 10
      tmp /= 10
    end

    self * pow + b
  end
end

total = []
for i in 1..99
  start = i > 9 ? 123 : 1234
  final = 10000 / i + 1
  for j in start..final
    res = i * j
    concated = res.concat(i).concat(j)
    if concated.is_pandigital?
      p concated
      total << res
    end
  end
end

p total.uniq.reduce(:+)

=begin rdoc
  
sum = []
for i in 1..99
  start = i > 9 ? 123 : 1234
  final = 10000 / i + 1
    
  for j in start..final
    res = i * j
    arr = i.digits + j.digits + res.digits

    if arr.uniq.size == 9 and arr.size == 9 and arr.reduce(:+) == 45
      p "#{i.digits}, #{j.digits}, #{res.digits}"
      sum << res 
    end
  end
end

p sum.uniq.reduce(:+)

=end

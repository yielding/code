#!/usr/bin/env ruby

require_relative "sieve"
require "pp"

class Numeric
  def digits
    self.to_s.scan(/./).map { |x| x.to_i }
  end

  def same_permutation? n
    arr = [0] * 10
    tmp = self 
    while tmp > 0
      arr[tmp % 10] += 1
      tmp /= 10
    end

    tmp = n
    while tmp > 0
      arr[tmp % 10] -= 1
      tmp /= 10
    end

    return arr.all? { |v| v == 0 }
  end
end

class Array
  def to_num
    self.reduce { |s, e| s*10 + e }
  end
end

base = eratosthenes_sieve(10000)
data = base.select { |n| n > 1498                }
           .select { |n| not n.digits.include? 0 }

for i in (0...data.size)
  for j in (i + 1...data.size)
    k = 2 * data[j] - data[i]
    next if k > 10000 or not data.include? k
    a = data[i]
    b = data[j]
    c = k
    if a.same_permutation?(b) and b.same_permutation?(c)
      pp "#{data[i]}#{data[j]}#{k}"
      exit
    end
  end
end

#!/usr/bin/env ruby19

require_relative "sieve"
require "pp"

class Numeric
  def digits
    self.to_s.scan(/./).map { |x| x.to_i }
  end
end

class Array
  def to_num
     self.reduce { |s, e| s*10 + e} 
  end
end

base = eratosthenes_sieve(10000)
data = base.select { |n| n > 1000                }
           .select { |n| not n.digits.include? 0 }
           .select { |n| n.digits.uniq.size == 4 }

data.each { |n|
  p n.digits.permutation.map.sort
     .map { |e| e.to_num }
}

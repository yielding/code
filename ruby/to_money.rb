#!/usr/bin/env ruby
# encoding: utf-8

require "pp"

class String
  def to_money
    len = length
    pos = 3 
    while (len - pos > 0)
      insert(len-pos, ',')
      pos += 3
    end
    self
  end
end

moneys = ["1", "10", "100", "1000", "10000", "100000", "100000000" ]
pp moneys.map { |money| money.to_money }

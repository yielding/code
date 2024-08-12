#!/usr/bin/env ruby
#encoding: utf-8

class Array
  def sum
    self.reduce(:+)
  end
end

p (1..10).select { |e| e < 5 }
         .map    { |e| e * e }
         .sum  

#!/usr/bin/env ruby

def fact(n)
  1.upto(n).reduce(:*)
end

res = fact(100) 
p res
p res.to_s.scan(/\d/).map {|e| e.to_i }.reduce(:+)

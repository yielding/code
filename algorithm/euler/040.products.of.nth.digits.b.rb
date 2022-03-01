#!/usr/bin/env ruby

require "numeric_ext"

sum = 0
res = []
vals = (0..6).map { |e| 10 ** e }
    
(1..200000).each { |e|
  count = e.digit_count

  count.times { |p|
    sum += 1
    res << e.to_s[p].to_i if vals.include?(sum)
  }
}

p res.reduce(:*)

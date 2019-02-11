#!/usr/bin/env ruby

require_relative "numeric_ext"

sum = 0
(0..9).to_a.permutation(10) { |e| 
  next if e[1].concat(e[2]).concat(e[3]) %  2 != 0
  next if e[2].concat(e[3]).concat(e[4]) %  3 != 0
  next if e[3].concat(e[4]).concat(e[5]) %  5 != 0
  next if e[4].concat(e[5]).concat(e[6]) %  7 != 0
  next if e[5].concat(e[6]).concat(e[7]) % 11 != 0
  next if e[6].concat(e[7]).concat(e[8]) % 13 != 0
  next if e[7].concat(e[8]).concat(e[9]) % 17 != 0
  sum += e.reduce(0) { |s, e| s * 10 + e }
}

p sum

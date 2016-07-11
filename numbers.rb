#!/usr/bin/env ruby

require "pp"

# p "10".each_char.map { |e| e.to_i }

# cnter = ->n { ([*1..n]*'').chars.reduce(Hash.new(0)) {|a,c| a[c]+=1; a} }

p ([*1..10] * '').chars.reduce(Hash.new(0)) { |a, c| a[c] += 1 ; a }

#p [*'1'..'10']

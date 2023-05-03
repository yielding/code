#!/usr/bin/env ruby

require "pp"

#cnter = ->n { ([*1..n]*'').chars.reduce(Hash.new(0)) {|a,c| a[c]+=1; a} }

p ([*1..10] * '').chars.reduce(Hash.new(0)) { |a, c| a[c] += 1 ; a }

#p [*'1'..'10']

#!/usr/bin/env ruby

data = ["I am a boy", "You are a girl", "We are good friends"]

dict = Hash.new { |h, key| h[key] = 0 }
data.each { |line| line.downcase.each_char { |ch| dict[ch] += 1 } }
dict.sort.each { |key, val|
  puts " #{key} - #{val} " if key =~ /[a-z]/
}

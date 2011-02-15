#!/usr/bin/env ruby -wKU
#-*- coding:utf-8 -*-

if ARGV.length == 0
  puts "uni_convert.rb 스트링"
  exit
end

s = ARGV.join
s.unpack("U*").each_with_index { |b, index|
  printf("(%X: %s) ", b, s[index])
  puts if (index+1) % 5 == 0
}

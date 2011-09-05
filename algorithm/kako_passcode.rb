#!/usr/bin/env ruby -wKU
require "digest/sha1"

sha1 = Digest::SHA1.new

0.upto(9999) { |n| 
  sha1.reset
  key = sprintf("%04d", n) 
  sha1 << key
  res = sha1.hexdigest

  puts "#{res} , #{key}"
}

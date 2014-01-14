#!/usr/bin/env ruby
# encoding: utf-8

lineno = `wc -l /Users/yielding/code/ruby/wc.rb`
puts lineno

puts Dir.glob("/Users/*")
puts Dir["/Users/*"]

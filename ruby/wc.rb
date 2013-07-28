#!/usr/bin/env ruby

lineno = `wc -l /Users/yielding/code/ruby/wc.rb`
puts lineno

puts Dir.glob("/Users/*")
puts Dir["/Users/*"]

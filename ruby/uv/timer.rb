#!/usr/bin/env ruby

require 'libuv'

reactor do |reactor|
  reactor.timer {
    puts "5 seconds passed"
  }.start(5000)
end

puts "reactor stopped. No more IO to process"

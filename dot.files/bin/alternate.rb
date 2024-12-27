#!/usr/bin/env ruby

if ARGV.length == 1
  `cp ~/bin/cpp.alternate/.projections.json #{ARGV[0]}` 
  return
end

puts "usage: alternate.rb ."

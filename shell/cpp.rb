#!/usr/bin/env ruby

if ARGV.length == 1
  `cp -r ~/bin/cpp.defualt ./#{ARGV[0]}` 
  return
end

puts "usage: cpp.rb project.name"

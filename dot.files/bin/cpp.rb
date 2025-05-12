#!/usr/bin/env ruby

if ARGV.length != 2
  puts 'Usage: ruby cpp.rb <compiler> <file>'
  return
end

if ARGV[0] == "clang++"
  `cp -r ~/bin/cpp.clang++ ./#{ARGV[1]}` 
else 
  `cp -r ~/bin/cpp.defualt ./#{ARGV[1]}` 
end

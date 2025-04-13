#!/usr/bin/env ruby

if ARGV.length == 1
  `cp -r ~/bin/cpp.defualt [clang++ | anyting]./#{ARGV[0]}` 
  return
end

if ARGV.length == 2
  if ARGV[0] == "clang++"
    `cp -r ~/bin/cpp.clang++ ./#{ARGV[1]}` 
  else 
    `cp -r ~/bin/cpp.defualt ./#{ARGV[1]}` 
  end
else 
  puts "usage: cpp.rb project.name"
end


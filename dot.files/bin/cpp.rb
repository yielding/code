#!/usr/bin/env ruby

if ARGV.length != 1
  puts 'Usage: cpp.rb <project_name>'
  return
end

`cp -r ~/bin/cpp.clang++ ./#{ARGV[0]}` 

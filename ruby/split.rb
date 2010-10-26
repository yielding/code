#!/usr/bin/env ruby

if ARGV[0] == nil
  puts "usage: split.rb filename.rb"
  exit
end

name = ARGV[0].split('.')[0]
f1   = File.open("#{name}_ap.txt", "w")
f2   = File.open("#{name}_cd.txt", "w")

File.open(ARGV[0]) do |f|
  f.readlines.each do |line|
    line.length > 40 ? f2.write(line) : f1.write(line)
  end
end

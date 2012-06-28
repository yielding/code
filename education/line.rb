#!/usr/bin/env ruby

fname = (ARGV.size == 1) ? ARGV[0] : 'line.rb'

File.open(fname) { |f|
  f.readlines.each.with_index { |line, index| puts "#{index+1} : #{line}" }
}

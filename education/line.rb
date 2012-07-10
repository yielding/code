#!/usr/bin/env ruby

fname = (ARGV.size == 1) ? ARGV[0] : 'line.rb'
ofile = File.open("#{fname}_res", 'w')

File.open(fname) { |ifile|
  ifile.readlines.each.with_index { |line, index| 
    res = "#{index+1} : #{line}"
    ofile.write(res)
  }
}

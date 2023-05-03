#!/usr/bin/env ruby

fname = (ARGV.size == 1) ? ARGV[0] : 'line_sort.rb'

File.open(fname) { |file|
  file.readlines.sort_by { |line| line.length }
      .each { |line| p line }
}

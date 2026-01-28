#!/usr/bin/env ruby

size = 20

1.upto(size) do |n|
  1.upto(size - n)  { print ' ' }
  1.upto(2 * n - 1) { print '*' }
  puts ''
end

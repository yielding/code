#!/usr/bin/env ruby

size = 20

1.upto(size) { |n|
  1.upto(size - n) { |k| print " " }
  1.upto(2*n - 1) { |k| print "*" }
  puts ""
}

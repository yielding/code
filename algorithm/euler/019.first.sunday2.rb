#!/usr/bin/env ruby19

require "date"

count = 0
for y in 1901..2000
  count += 1.upto(12).count { |m| Date.new(y, m, 1).wday == 0 }
end

p count

p (1901..2000).map { |y| 1.upto(12).count { |m| Date.new(y, m, 1).wday == 0 } } \
              .reduce(:+)

p (1901..2000).map.reduce(0) { |s, y| 
  s + 1.upto(12).count { |m| Date.new(y, m, 1).wday == 0 } }

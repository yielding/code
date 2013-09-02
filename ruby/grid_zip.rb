#!/usr/bin/env ruby

a = 3.times.map { gets.split }
p a
p (a + a.transpose).map { |l| l.inject(0) { |s, i| s+i.to_i } }.max

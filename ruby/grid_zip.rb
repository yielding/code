#!/usr/bin/env ruby
# encoding: utf-8

a = 3.times.map { gets.split }
p a
p (a + a.transpose).map { |l| l.inject(0) { |s, i| s+i.to_i } }.max

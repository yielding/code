#!/usr/bin/env ruby2.0

require "set"

n = 4
a = [*0...n]
a.permutation.each { |c| 
  s1 = Set.new(c.zip(a).map { |e| e.reduce(:+) }) 
  s2 = Set.new(c.zip(a).map { |e| e.reduce(:-) })
  p c if [s1, s2].all? { |x| x.size == n }
}

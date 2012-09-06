#!/usr/bin/env ruby -wKU
require "set"

n = 4
a = [*0...n]
a.permutation.each { |l| 
  s1 = Set.new l.zip(a).map { |e| e.reduce(:+) }
  s2 = Set.new l.zip(a).map { |e| e.reduce(:-) }
  p l if s1.size == n and  s2.size == n
}

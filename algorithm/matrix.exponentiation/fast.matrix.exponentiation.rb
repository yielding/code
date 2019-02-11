#!/usr/bin/env ruby

require "pp"
require "matrix"

$data = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
$dim  = 3

def solve p, m
  return m   if p == 1 
  return m*m if p == 2

  m2 = solve(p / 2, m)

  return m2 * m2 if p.even?
  return m2 * m2 * m 
end

arr = $data.each_slice($dim).map { |c| c }
m1  = Matrix[*arr]

t0 = Time.new
pp solve(500000, m1).map { |e| e % 10007 }
p Time.now - t0

t0 = Time.new
pp (m1 ** 500000).map { |e| e % 10007 }
p Time.now - t0

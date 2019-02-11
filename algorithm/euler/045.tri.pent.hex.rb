#!/usr/bin/env ruby
require "pp"

tris = (1..10).map { |n| n * (  n + 1) / 2 }
pent = (1..10).map { |n| n * (3*n - 1) / 2 }
hexa = (1..10).map { |n| n * (2*n - 1)     }

pp tris
pp pent
pp hexa

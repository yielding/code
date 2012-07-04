#!/usr/local/bin/macruby

require 'dispatch'

map_reduce = (0..1_000_000).p_mapreduce(0) { |n| Math.sqrt(n) }
p map_reduce

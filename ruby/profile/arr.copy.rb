#!/usr/bin/env ruby
# encoding: utf-8

$arr = [*1..100000]

def copy
  sub = $arr[10..-1]
end

for i in 0..1000
  copy
end

#!/usr/bin/env ruby

def max(arr)
  arr.reduce { |biggest, cur| cur > biggest ? cur : biggest }
end

p max([1, 3, 5, 8, 7])

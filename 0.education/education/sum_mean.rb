#!/usr/bin/env ruby -wKU

class Array 
  def sum
    reduce(0.0, :+)
  end

  def mean
    sum / size
  end
end

arr = []
10.times { 
  arr << Random.rand(100) 
  puts "arr: #{arr.to_s}"
  printf "sum: %d\navr: %5.2f\n", arr.sum, arr.mean
}


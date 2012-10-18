#!/usr/bin/env ruby19

require "pp"
require_relative "numeric_ext"

module Enumerable
  def to_num
    self.reduce(0) { |product, v| product * 10 + v }
  end
end

SIZE   = 500
$table = (0..SIZE).map { |e| [0] * SIZE }

def partition sum, largest_no
  def partition_impl sum, ln, arr, res
    if sum == 0
      pp arr
      res << arr
      return 1
    end

    return 0 if ln == 0 or sum < 0

    unless $table[sum][ln].zero?
      #pp "return #{sum}, #{ln}"
      return $table[sum][ln]
    end

    $table[sum][ln] = 
      partition_impl(sum, ln - 1, arr, res) + 
      partition_impl(sum - ln, ln, arr.clone.push(ln), res)

    return $table[sum][ln]
  end

  res = []
  pp partition_impl(sum, largest_no, [], res)
  res
end

pp partition(13*13, 9)

#!/usr/bin/env ruby19

require "pp"
require_relative "numeric_ext"

module Enumerable
  def to_num
    self.reduce(0) { |product, v| product * 10 + v }
  end
end

$table = (0..100).map { |e| [0] * 100 }

def partition sum, largest_no
  return 0 if largest_no == 0 or sum < 0
  return 1 if sum == 0
  return $table[sum][largest_no] unless $table[sum][largest_no].zero?

  $table[sum][largest_no] = 
    partition(sum, largest_no - 1) + 
    partition(sum - largest_no, largest_no)

  return $table[sum][largest_no]
end

pp partition(5, 5)


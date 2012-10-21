#!/usr/bin/env ruby19

require "pp"
require_relative "numeric_ext"

module Enumerable
  def to_num
    self.reduce(0) { |product, v| product * 10 + v }
  end
end

$coins = [*1..5]

def count_change amount, arr
  def cc amount, kinds_of_coin, arr, res
    if amount.zero?
      pp arr
      res << arr
      return 1
    end

    return 0 if amount < 0 or kinds_of_coin.zero?

    cc(amount, kinds_of_coin - 1, arr, res) + 
    cc(amount - $coins[kinds_of_coin-1], kinds_of_coin, 
       arr.clone.push(kinds_of_coin), res)
  end

  res = []
  cc(amount, 5, arr, res)
  res
end

res = count_change 5, []
<<<<<<< HEAD
pp res
#xxx = res.map { |arr| arr.permutation.map { |per| per }.uniq }
#arr = xxx.uniq.reduce(:+)
#p arr
#p arr.map { |e| e.to_num }.sum
=======
p res

# xxx = res.map { |arr| arr.permutation.map { |per| per }.uniq }
# arr = xxx.uniq.reduce(:+)
# p arr
# p arr.map { |e| e.to_num }.sum
>>>>>>> 656166f32f37f0b72a917c78892f31b1ba29fe7e

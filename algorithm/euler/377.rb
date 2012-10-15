#!/usr/bin/env ruby19

$coins = [ 1,  2,  3,  4,  5]

def count_change amount, arr
  def cc amount, kinds_of_coin, arr, res
    if amount.zero?
      res << arr
      return 1
    end

    if amount < 0 or kinds_of_coin.zero?
      return 0
    end

    cc(amount, kinds_of_coin.pred, arr.clone, res) + 
    cc(amount- $coins[kinds_of_coin-1], kinds_of_coin, arr.clone.push(kinds_of_coin), res)
  end

  res = []
  cc(amount, 5, arr, res)
  res
end

res = count_change 5, []
xxx = res.map { |arr| arr.permutation.map { |per| per }.uniq }
p xxx.uniq.reduce(:+)

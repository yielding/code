#!/usr/bin/env ruby

# $coins = { 1 => 1, 2 => 2, 3 => 5, 4 => 10, 5 => 20, 6 => 50, 7 => 100, 8 => 200 }
$coins = [ 1,  2,  5,  10,  20,  50,  100,  200 ]

def count_change amount
  def cc amount, kinds_of_coin
    return 1 if amount.zero?
    return 0 if amount < 0 or kinds_of_coin.zero?

    cc(amount, kinds_of_coin.pred) + 
    cc(amount-$coins[kinds_of_coin-1], kinds_of_coin)
  end

  cc(amount, 8)
end

p count_change 200

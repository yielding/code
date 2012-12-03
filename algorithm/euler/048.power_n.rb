#!/usr/bin/env ruby19

class Numeric
  def power_n
    self ** self
  end
end

p (1..1000).map { |n| n.power_n }.reduce(:+) % 10000000000

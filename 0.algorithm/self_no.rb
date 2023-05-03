#!/usr/bin/env ruby

require "set"
require "pp"

class Numeric
  def digits
    self.to_s.scan(/./).map { |d| d.to_i }
  end

  def gen
    self + self.digits.reduce(:+)
  end
end

self_nums = [*1..5000] - [*1..5000].map { |d| d.gen }
p self_nums.reduce :+

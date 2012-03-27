#!/usr/bin/env ruby -KU

require "test/unit"

class Numeric
  def sum_upto b
    return (self == b) ? b : self + (self + 1).sum_upto(b)
  end
end

class TestSum < Test::Unit::TestCase
  def test_sum
    assert_equal(10,  10.sum_upto(10))
    assert_equal(55,   1.sum_upto(10))
    assert_equal(5050, 1.sum_upto(100))
  end
end

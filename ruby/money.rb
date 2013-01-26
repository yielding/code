#!/usr/bin/env ruby1.9

require "test/unit"

class Fixnum
  def to_money(sep=",", width=3)
    res = self.to_s
    pos = res.length - width
    while pos > 0
      res.insert(pos, sep)
      pos -= 3
    end
    res
  end
end

class TestMoney < Test::Unit::TestCase
  def test_3
    assert_equal(     10.to_money, "10")
    assert_equal(    100.to_money, "100")
    assert_equal(   1000.to_money, "1,000")
    assert_equal(  10000.to_money, "10,000")
    assert_equal( 100000.to_money, "100,000")
    assert_equal(1000000.to_money, "1,000,000")
  end
end

#!/usr/bin/env ruby

require "test/unit"

class Fixnum
  def to_money(width:3, sep:",")
    res = self.to_s
    pos = res.length - width
    while pos > 0
      res.insert(pos, sep)
      pos -= width
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

  def test_4
    assert_equal(     10.to_money(width:4), "10")
    assert_equal(    100.to_money(width:4), "100")
    assert_equal(   1000.to_money(width:4), "1000")
    assert_equal(  10000.to_money(width:4), "1,0000")
    assert_equal( 100000.to_money(width:4), "10,0000")
    assert_equal(1000000.to_money(width:4), "100,0000")
  end
end

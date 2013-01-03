#!/usr/bin/env ruby19

require 'test/unit'

require_relative 'money'

class TestMoney < Test::Unit::TestCase
  def setup
  end

  def teardown
  end

  def test_dollar_multiplication
    five = Money.dollar(5)
    assert(Money.dollar(10) == five * 2)
    assert(Money.dollar(15) == five * 3)
  end

  def test_equality
    assert(Money.dollar(5) == Money.dollar(5))
    assert(Money.dollar(5) != Money.dollar(6))

    assert(Money.franc(5) == Money.franc(5))
    assert(Money.franc(5) != Money.franc(6))
  end

  def test_currency
    assert_equal("USD", Money.dollar(1).currency())
    assert_equal("CHF", Money.franc(1).currency())
  end

  def test_simple_addition
    five = Money.dollar(5)
    sum  = five + five
    bank = Bank.new
    reduced = bank.reduce(sum, "USD")
    assert_equal(Money.dollar(10), reduced)
  end

  def test_plus_retun_sum
    five = Money.dollar(5)
    sum  = five + five
    assert_equal(five, sum.augend)
    assert_equal(five, sum.addend)
  end

  def test_reduce_sum
    sum = Sum.new(Money.dollar(3), Money.dollar(4))
    bank = Bank.new
    result = bank.reduce(sum, "USD")
    assert_equal(Money.dollar(7), result)
  end

  def test_reduce_money
    bank = Bank.new
    result = bank.reduce(Money.dollar(1), "USD")
    assert_equal(Money.dollar(1), result)
  end

  def test_reduce_money_different_currency
    bank = Bank.new
    bank.add_rate("CHF", "USD", 2);
    result = bank.reduce(Money.franc(2), "USD");
    assert_equal(Money.dollar(1), result)
  end

  def test_identity
    bank = Bank.new;
    assert_equal(1, bank.rate("USD", "USD"))
  end

  def test_mixed_addtion
    five_bucks = Money.dollar(5)
    ten_franc  = Money.franc(10)
    bank = Bank.new
    bank.add_rate("CHF", "USD", 2)
    result = bank.reduce(five_bucks + ten_franc, "USD")
    assert_equal(Money.dollar(10), result)
  end

  def test_sum_plus_money
    five_bucks = Money.dollar(5)
    ten_franc  = Money.franc(10)
    bank = Bank.new
    bank.add_rate("CHF", "USD", 2)
    sum = Sum.new(five_bucks, ten_franc) + five_bucks
    result = bank.reduce(sum, "USD")
    assert_equal(Money.dollar(15), result)
  end

  def test_sum_times
    five_bucks = Money.dollar(5)
    ten_franc  = Money.franc(10)
    bank = Bank.new
    bank.add_rate("CHF", "USD", 2)
    sum = Sum.new(five_bucks, ten_franc) * 2
    result = bank.reduce(sum, "USD")
    assert_equal(Money.dollar(20), result)
  end

  def test_plus_same_currency_return_money
    sum = Money.dollar(1) + Money.dollar(1)
    assert(sum.is_a?(Expression))
  end
end

#!/usr/bin/env ruby
# frozen_string_literal: true

require 'test/unit'

# Computes Pascal's triangle values and renders rows as formatted text.
class Pascal
  def value_of(row, col)
    return 0 if row < 1 || col < 1 || col > row
    return 1 if row == 1

    value_of(row - 1, col - 1) + value_of(row - 1, col)
  end

  def values_of(row)
    return [] if row.zero?

    res = []
    1.upto(row) { |col| res << value_of(row, col) }
    res
  end

  def line_of(row)
    res = ''
    1.upto(row) { |col| res += format('%6d', value_of(row, col)) }
    res
  end

  def lines_upto(row)
    res = ''
    1.upto(row) { |r| res += "#{' ' * (row - r) * 3}#{line_of(r)}\n" }
    res
  end
end

# Unit tests for Pascal.
class TestPascal < Test::Unit::TestCase
  def setup
    @p = Pascal.new
  end

  def test_value_of
    expected = {
      [1, 1] => 1, [1, 2] => 0, [2, 1] => 1, [2, 2] => 1, [3, 2] => 2,
      [3, 3] => 1, [4, 2] => 3, [4, 3] => 3, [6, 3] => 10
    }

    expected.each do |(row, col), value|
      assert_equal(value, @p.value_of(row, col))
    end
  end

  def test_values_of
    assert_equal([], @p.values_of(0))
    assert_equal([1], @p.values_of(1))
    assert_equal([1, 1], @p.values_of(2))
    assert_equal([1, 2, 1], @p.values_of(3))
    assert_equal([1, 3, 3, 1], @p.values_of(4))
    assert_equal([1, 4, 6, 4, 1], @p.values_of(5))
  end

  def test_values_upto
    puts ''
    puts @p.lines_upto(10)
  end
end

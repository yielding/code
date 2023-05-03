#!/usr/bin/env ruby

require 'test/unit'


class Pascal
  def initialize
    @map = Hash.new { |h, k| h[k] = Hash.new(-1) }
  end

  def value_of2(x, y)
    return 0 if x < 1 or y < 1 or y > x
    return 1 if x == 1

    @map[x-1][y-1] = value_of(x-1, y-1) if @map[x-1][y-1] == -1
    @map[x-1][y  ] = value_of(x-1, y  ) if @map[x-1][y  ] == -1

    @map[x-1][y-1] + @map[x-1][y]
  end

  def value_of(x, y)
    return 0 if x < 1 or y < 1 or y > x
    return 1 if x == 1 
    value_of(x-1, y-1) + value_of(x-1, y)
  end
  
  def values_of(row)
    row == 0 ? [] : 1.upto(row).map { |col| value_of(row, col) }
  end
  
  def line_of(row)
    1.upto(row).reduce("") {|sum, col| sum + sprintf("%6d", value_of(row, col)) }
  end
  
  def lines_upto(row)
    1.upto(row).reduce("") { |sum, r| sum + " " * (row-r)*3 + line_of(r) + "\n" }
  end
end

class TestPascal < Test::Unit::TestCase
  def setup
    @p = Pascal.new
  end

  def test_value_of
    assert_equal(1, @p.value_of(1, 1))
    assert_equal(0, @p.value_of(1, 2))
    assert_equal(1, @p.value_of(2, 1))
    assert_equal(1, @p.value_of(2, 2))
    assert_equal(2, @p.value_of(3, 2))
    assert_equal(1, @p.value_of(3, 3))
    assert_equal(3, @p.value_of(4, 2))
    assert_equal(3, @p.value_of(4, 3))
    assert_equal(10, @p.value_of(6, 3))
  end

  def test_values_of
    assert_equal([], @p.values_of(0))
    assert_equal([1], @p.values_of(1))
    assert_equal([1,1], @p.values_of(2))
    assert_equal([1, 2, 1], @p.values_of(3))
    assert_equal([1, 3, 3, 1], @p.values_of(4))
    assert_equal([1, 4, 6, 4, 1], @p.values_of(5))
  end
  
  def test_values_upto
    puts ""
    puts @p.lines_upto(10)
  end
end

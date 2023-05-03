#!/usr/bin/env ruby

require "test/unit"

class SpiralArray
  attr_reader :arr

  def initialize(size)
    @size = size 
    @arr  = size.times.map { [0] * size }
  end

  def go
    x, y = @size / 2, @size / 2
    no, step = 1, 1
    dx, dy   = 1, 0
    toggle   = true

    while no <= @size * @size
      step.times { 
        @arr[y][x] = no
        x, y, no = x + dx, y + dy, no + 1
      }

      dx, dy = dy, dx
      toggle = !toggle
      step, dx, dy = step + 1, -dx, -dy if toggle
    end
  end

  def row_at index
    (0...@size).map { |y| @arr[index][y]}
  end

  def sum_of_diagonals
    s1 = (0...@size).map { |i| @arr[i][i] }.reduce(:+)
    s2 = (0...@size).map { |i| @arr[@size-i-1][i] }.reduce(:+)
    return s1 + s2 - @arr[@size/2][@size/2] 
  end
end

class TestSpiralArray < Test::Unit::TestCase
  def test_5by5
    sa = SpiralArray.new(5)
    sa.go
    assert_equal(sa.row_at(0), [21, 22, 23, 24, 25])
    assert_equal(sa.row_at(1), [20,  7,  8,  9, 10])
    assert_equal(sa.row_at(2), [19,  6,  1,  2, 11])
    assert_equal(sa.row_at(3), [18,  5,  4,  3, 12])
    assert_equal(sa.row_at(4), [17, 16, 15, 14, 13])
    assert_equal(sa.sum_of_diagonals, 101)
  end

  def test_1001by1001
    sa = SpiralArray.new(1001)
    sa.go
    assert_equal(sa.sum_of_diagonals, 669171001)
  end
end

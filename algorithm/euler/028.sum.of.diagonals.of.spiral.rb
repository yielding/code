#!/usr/bin/env ruby19

require "test/unit"
require "pp"

class SpiralArray
  attr_reader :arr

  def initialize(size)
    @size = size 
    @arr  = size.times.map { [0] * size }
  end

  def go
    x, y = @size / 2, @size / 2
    no   = 1
    step = 1
    dir  = 1

    while no <= @size * @size
      2.times { |i|
        step.times { 
          @arr[y][x] = no
          x  += dir if i == 0 
          y  += dir if i == 1 
          no += 1
        }
        break if no > @size * @size
      }

      step +=  1
      dir  *= -1
    end
  end

  def sum_of_diagonals
    s1, s2 = 0, 0
    for i in 0...@size
      s1 += @arr[i][i]
      s2 += @arr[@size - i - 1][i]
    end
    s1 + s2 - @arr[@size/2][@size/2] 
  end
end

class TestSpiralArray < Test::Unit::TestCase
  def setup
    @sa = SpiralArray.new(1001)
    @sa.go
    p @sa.sum_of_diagonals
  end

  def test_ok
    assert_equal(1, 1)
  end
end

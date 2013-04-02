#!/usr/bin/env ruby2.0

class MaxPath
  attr_reader :max, :grid

  def initialize iter, size
    @size = size
    @grid = 0.upto(size-1).map { |e| (e+1).times.map { iter.next.to_i } }
    @max  = 0
  end

  def solve_dynamic
    max = @grid[@size-1].clone

    (@size-2).downto(0) { |r|
      0.upto(r) { |c| max[c] = @grid[r][c] + [max[c], max[c+1]].max }
    }

    max[0]
  end
end

if __FILE__ == $PROGRAM_NAME
  grid = File.foreach("./triangle.txt").map { |l|
    l.chomp!.split
  }.flatten

  mp = MaxPath.new(grid.to_enum, 100)
  p mp.solve_dynamic
end

=begin rdoc
require "test/unit"

class TestMaxPath < Test::Unit::TestCase
  def setup
    @t = MaxPath.new($tri.to_enum, 15)
  end

  def test_solve_dynamic
    seq = [75, 64, 82, 87, 82, 75, 73, 28, 83, 32, 91, 78, 58, 73, 93]
    val = @t.solve_dynamic
    assert_equal(val, seq.reduce(:+))
    # assert_equal(seq, @t.seq)
  end

  def test_grid_at
    assert_equal(75, @t.grid[0].first)
    assert_equal([95, 64], @t.grid[1].first(2))
    assert_equal([4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23], 
                 @t.grid[14].first(15))
  end
end
=end

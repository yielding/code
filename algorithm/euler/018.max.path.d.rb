#!/usr/bin/env ruby19

require "test/unit"

$tri = %w{     
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
}

class MaxPath
  attr_reader :grid

  def initialize iter, size
    @size = size
    @grid = 0.upto(size-1).map { |e| (e+1).times.map { iter.next.to_i } }
  end

  def solve_dynamic
    max = @grid[@size-1].clone

    (@size-2).downto(0) { |r|
      0.upto(r) { |c| max[c] = @grid[r][c] + [max[c], max[c+1]].max }
    }

    max[0]
  end

end

class TestMaxPath < Test::Unit::TestCase
  def setup
    @t = MaxPath.new($tri.to_enum, 15)
  end

  def test_solve_dynamic_part
    tt = MaxPath.new($tri.to_enum, 3)
    seq = [75, 64, 82, 87, 82, 75, 73, 28, 83, 32, 91, 78, 58, 73, 93]
    val = tt.solve_dynamic
    p val
    # assert_equal(seq, @t.seq)
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

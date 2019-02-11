#!/usr/bin/env ruby

require "test/unit"

$tri = %w{     75
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

class MaxTriangle
  attr_reader :max
  attr_reader :count
  attr_reader :seq

  def initialize iter, size
    @size = size
    @grid = 0.upto(size-1).map { |e| (e+1).times.map { iter.next.to_i } }
    @max, @count = 0, 0
  end

  def solve
    backtrack([], 0, 0)
    @max
  end

  def backtrack(arr, row, col)
    arr.size == @size-1 \
      ? process(arr)
      : candidates(row, col).each_with_index { |v, c| 
          backtrack(arr.clone.push(v), row+1, col + c) 
        }
  end

  def process arr
    arr.unshift(@grid[0][0])
    sum = arr.reduce(:+)

    @max, @seq = sum, arr.clone if sum > @max
    @count += 1
  end

  def row_at(index)
    @grid[index]
  end

  def candidates(row, col)
    raise IndexError, "row out of bound" if row > @size - 2

    r = @grid[row+1]
    return [r[col], r[col+1]]
  end
end

class TestMaxTriangle < Test::Unit::TestCase
  def setup
    @t = MaxTriangle.new($tri.to_enum, 15)
  end

  def test_solve
    @t.solve
    seq = [75, 64, 82, 87, 82, 75, 73, 28, 83, 32, 91, 78, 58, 73, 93]
    assert_equal(@t.solve, seq.reduce(:+))
    assert_equal(seq, @t.seq)
  end

  def test_candidates
    assert_equal([95, 64], @t.candidates(0, 0))
    assert_equal([17, 47], @t.candidates(1, 0))

    assert_raise IndexError do
      @t.candidates(15, 0)
    end
  end

  def test_row_at
    assert_equal(75, @t.row_at(0).first)
    assert_equal([95, 64], @t.row_at(1).first(2))
    assert_equal([4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23], 
                 @t.row_at(14).first(15))
  end
end

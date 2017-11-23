#!/usr/bin/env ruby

require "test/unit"
require "set"

class FourBox
  def initialize(s)
    @boxes = s.each_line.map { |l| l.scan(/\d+/).map { |d| d.to_i } }
  end

  def area
    s = Set.new
    @boxes.each do |x1, y1, x2, y2|
      x1.upto(x2-1) { |x| y1.upto(y2-1) { |y| s.add([x, y]) } }
    end
    s.size
  end
end

class TestFourBox < Test::Unit::TestCase
  def test_1
    boxes = FourBox.new(
    "0 0 200 200
     0 0 200 200
     0 0 200 200
     0 0 200 200")
    assert_equal(boxes.area, 40000)
  end

  def test_2
    boxes = FourBox.new(
    "1 2 4 4
     2 3 5 7
     3 1 6 5
     7 3 8 6")
    assert_equal(boxes.area, 26)
  end
end

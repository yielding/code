#!/usr/bin/env ruby1.9

require "pp"

class Point
  attr_accessor :x, :y 

  def initialize(x, y)
    @x, @y = x, y
  end

  def rel_to(p)
    Point.new(@x - p.x, @y - p.y)
  end

  def make_rel_to(p)
    @x -= p.x
    @y -= p.y
  end

  def translate(x0, y0)
    Point.new(@x + x0, @y + y0)
  end

  def reversed
    Point.new(-@x, -@y)
  end

  def is_lower(p)
    return @y < p.y || @y == p.y && @x < p.x 
  end

  def == rhs
    @x == rhs.x && @y == rhs.y
  end

  def mdist_
    @x.abs + @y.abs
  end

  def mdist(p)
    rel_to(p).mdist_
  end

  def is_further(p)
    self.mdist_ > p.mdist_
  end

  def is_between(p0, p1)
    p0.mdist(p1) >= mdist(p0) + mdist(p1)
  end

  def is_less(p)
    f = self.cross(p)
    f > 0 || (f == 0 and is_further(p))
  end

  def area2(p0, p1)
    p0.rel_to(self).cross(p1.rel_to(self))
  end

  def is_convex(p0, p1)
    f = area2(p0, p1)
    f < 0 || (f == 0 and !is_between(p0, p1))
  end

  def dist
    Math.hypot(@x, @y) 
  end

  def dist_to p
    Math.hypot(@x - p.x, @y - p.y)
  end

  def inner p
    @x * p.x + @y * p.y
  end

  def cross p
    @x * p.y - @y * p.x
  end

  def swap other
    @x, other.x = other.x, @x
    @y, other.y = other.y, @y
  end
end

class JarvisMarch
  attr_reader :h
  attr_reader :p

  def compute_hull p
    @p = p
    @n = p.size
    @h = 0
    calculate
    @h
  end

  private
  def calculate
    i = index_of_lowest_point
    begin
      exchange(@h, i)
      i = index_of_right_most_point_from(@p[@h])
      @h += 1
    end while i > 0
  end

  def index_of_right_most_point_from q
    i = 0
    for j in 1...@n
      i = j if @p[j].rel_to(q).is_less(@p[i].rel_to(q)) 
    end
    i
  end

  def index_of_lowest_point
    min_idx = 0
    for i in 1...@n
      if @p[i].y < @p[min_idx].y || @p[i].y == @p[min_idx].y && @p[i].x < @p[min_idx].x
        min_idx = i
      end
    end

    min_idx
    # @p.each_with_index.reduce(0.0) do |min_idx, (v, i)|
    #   @p[min_idx].y  > v.y || 
    #   @p[min_idx].y == v.y && @p[min_idx].x < @p[i].x ? i : min_idx
    # end
  end

  def exchange i, j
    t = @p[i]
    @p[i] = @p[j]
    @p[j] = t
  end
end

require "test/unit"

class TestHull < Test::Unit::TestCase
  def setup
    @p0 = Point.new(3, 0)
    @p1 = Point.new(5, 2)
    @p2 = Point.new(3, 4)
    @p3 = Point.new(1, 2)
    @p4 = Point.new(2, 2)
    @p5 = Point.new(3, 2)
    @p  = [@p0, @p1, @p2, @p3, @p4, @p5]
  end

  def test_hull
    hull = JarvisMarch.new
    sz = hull.compute_hull(@p)
    for pt in 0...sz
      p hull.p[pt]
    end
  end
end

class TestPoint < Test::Unit::TestCase
  def setup
    @p0 = Point.new(3, 0)
    @p1 = Point.new(5, 2)
    @p2 = Point.new(3, 4)
    @p3 = Point.new(1, 2)
  end

  def test_area2
    assert_equal(@p1.area2(@p2, @p0), 8)
  end

  def test_is_less
    assert(@p1.is_further(@p0))
    assert(@p0.is_less(@p1))
  end

  def test_is_convex
    assert(@p2.is_convex(@p1, @p0))
    assert(!@p0.is_convex(@p1, @p2))
  end

  def test_is_lower
    assert(@p0.is_lower(@p1))
    assert(@p3.is_lower(@p1))
  end

  def test_distance
    assert_equal(@p0.dist_to(@p1), Math.sqrt(8))
  end

  def test_cross_product
    assert_equal(@p0.cross(@p1), 6)
  end

  def test_reversed
    assert_equal(Point.new(1, 1), Point.new(-1, -1).reversed)
  end

  def test_recoordinate
    p = @p0.clone
    assert_equal(p, @p0)
    assert_not_same(p, @p0)
  end

  def test_swap
    p0 = @p0.clone 
    p = Point.new(0, 0)
    p.swap(@p0)
    assert_equal(p, p0)
  end

  def test_if
    a = 10
    a = if a > 10; 11 else 23 end

    assert_equal(a, 23)
  end
end

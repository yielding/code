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
    @y < p.y || @y == p.y && @x < p.x 
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

  def to_s
    "x: #{x}, y: #{y}"
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
    @p.each_with_index.reduce(0.0) { |mi, (v, i)|
       @p[mi].y > v.y || (@p[mi].y == v.y && @p[mi].x < @p[i].x) ? i : mi
    }
  end

  def exchange i, j
    @p[i], @p[j] = @p[j], @p[i]
  end
end

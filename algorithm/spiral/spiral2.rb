require "test/unit"
require "pp"

class SpiralArray
  WALL  = -2
  SPACE = -1
  DIR   = { :e => { dx:  1, dy: 0, d: :s }, :s => { dx: 0, dy:  1, d: :w}, 
            :w => { dx: -1, dy: 0, d: :n }, :n => { dx: 0, dy: -1, d: :e} }
  
  def initialize w, h
    @w, @h, @x, @y, @value = w, h, 1, 1, 1
    @dir = :e
    @dx, @dy = DIR[@dir][:dx], DIR[@dir][:dy]
  end
  
  def board
    @board ||= begin
      board = []
      0.upto(@h+1) { |i| board.push([WALL] * (@w + 2)) }
      1.upto(@h)   { |y| 1.upto(@w) { |x| board[y][x] = SPACE } }
      board
    end
  end
  
  def row_at index
    board[index].join(", ")
  end
  
  def completed?
    @value > @w * @h;
  end
  
  def blocked?
    board[@y+@dy][@x+@dx] != SPACE
  end
  
  def go
    until completed?
      board[@y][@x] = @value
      if blocked?
        @dir     = DIR[@dir][:d]
        @dx, @dy = DIR[@dir][:dx], DIR[@dir][:dy]
      end
      @x += @dx
      @y += @dy
      @value += 1
    end
  end
  
end

class TestSpiralArray2 < Test::Unit::TestCase
  def setup
    @sa33 = SpiralArray.new(3, 3)
    @sa33.go
  end
  
  def test_3by3
    assert_equal("-2, -2, -2, -2, -2", @sa33.row_at(0))
    assert_equal("-2, 1, 2, 3, -2",    @sa33.row_at(1))
    assert_equal("-2, 8, 9, 4, -2",    @sa33.row_at(2))
    assert_equal("-2, 7, 6, 5, -2",    @sa33.row_at(3))
    assert_equal("-2, -2, -2, -2, -2", @sa33.row_at(4))
  end
end

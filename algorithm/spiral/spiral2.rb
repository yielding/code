require "test/unit"
require "pp"

class SpiralArray
  WALL  = -2
  SPACE = -1
  DIR = { :e => [ 1,  0, :s], :s => [ 0,  1, :w], 
          :w => [-1,  0, :n], :n => [ 0, -1, :e] }
  
  def initialize w, h
    @w, @h = w, h
    @x, @y, @value = 1, 1, 1
    @dir = :e
    @board = []
    init_board
  end
  
  def init_board
    0.upto(@h+1) { |i| @board.push([WALL] * (@w + 2)) }
    1.upto(@h+0) { |y| 1.upto(@w) { |x| @board[y][x] = SPACE } }
  end
  
  def row_at index
    @board[index].join(", ")
  end
  
  def completed?
    @value > @w * @h;
  end
  
  def blocked?
    # puts "#{@y}, #{@x}, #{@value}, #{@dir}"
    n = @board[@y+DIR[@dir][1]][@x+DIR[@dir][0]]
    return n == WALL || n != SPACE
  end
  
  def go
    while not completed?
      @board[@y][@x] = @value
      @value += 1
      @dir = DIR[@dir][2] if blocked?
      @x  += DIR[@dir][0]
      @y  += DIR[@dir][1]
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

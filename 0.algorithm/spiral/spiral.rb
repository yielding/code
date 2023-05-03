require "test/unit"
require "pp"

class SpiralArray
  SPACE = -1
  WALL  = -2
  DIR_SEQ  = [ [1, 0], [0, 1], [-1, 0], [0, -1] ]

  def initialize(width, height)
    @inc_x  = 1
    @inc_y  = 0
    @cur_x  = @cur_y = 1
    @value  = 1
    @width  = width
    @height = height
    @seq_index = 0
    @board  = []
    init_board
  end
  
  def init_board
    0.upto(@height + 1) do @board.push([WALL] * (@width + 2)) end
    1.upto(@height) do |y| 
      1.upto(@width) { |x| @board[y][x] = SPACE } 
    end
  end

  def go
    while @value <= @width * @height
      @board[@cur_y][@cur_x] = @value
      @value += 1
      update_position
    end
    self
  end

  def should_change_dir?
    next_value = @board[@cur_y + @inc_y][@cur_x + @inc_x];
    return next_value == WALL || next_value != SPACE
  end
  
  def update_position
    if should_change_dir?
      @seq_index = (@seq_index + 1) % 4
      @inc_x, @inc_y = DIR_SEQ[@seq_index]
    end
    
    @cur_x += @inc_x;
    @cur_y += @inc_y;
  end
  
  def to_arr
    res = ""
    1.upto(@height)  { |i|
      1.upto(@width) { |j| res += sprintf("%d ", @board[i][j]) }
    }
    res
  end
  
end

class TestSpiralArray < Test::Unit::TestCase
  def test_one_by_one
    sa = SpiralArray.new(1, 1).go
    assert_equal("1 ", sa.to_arr)
  end

  def test_2_by_2
    sa = SpiralArray.new(2, 2).go
    assert_equal("1 2 4 3 ", sa.to_arr)
  end
  
  def test_3by3
    sa = SpiralArray.new(3, 3).go
    assert_equal("1 2 3 8 9 4 7 6 5 ", sa.to_arr)
  end
end
#!/usr/bin/env ruby
# encoding: UTF-8

class MagicSquare
  def initialize dim
    @dim = dim
    @mat = Array.new(dim) { Array.new(dim) { 0 } }
    @num = 1
    @x = @dim / 2
    @y = 0
  end
  
  def n(v); (v + 1       ) % @dim end
  def p(v); (v - 1 + @dim) % @dim end

  def go
    while @num <= @dim ** 2
      @mat[@y][@x] = @num
      @num += 1
      @x, @y = left_top_of(@y, @x) == 0 \
        ? [n(@x), p(@y)] 
        : [  @x,  n(@y)]
    end
  end
  
  def left_top_of(y, x)
    @mat[p(y)][n(x)]
  end
  
  def print
    @mat.each { |arr| 
      arr.each { |e| printf("%3d ", e) } 
      puts ""
    }
  end
end

if __FILE__ == $PROGRAM_NAME
  ms = MagicSquare.new(13)
  ms.print
  ms.go
  ms.print
end

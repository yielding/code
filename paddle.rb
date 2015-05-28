#!/usr/bin/env ruby

require 'curses' 
include Curses

class Paddle
  HEIGHT = 4
  PADDLE = " \n" + "|\n"*HEIGHT + " " 

  def initialize
    @top = (Curses::lines - HEIGHT)/2
    draw
  end 

  def up
    @top -= 1 if @top > 1 
  end

  def down
    @top += 1 if (@top + HEIGHT + 1) < lines
  end

  def draw
    setpos(@top-1, 0)
    addstr(PADDLE)
    refresh
  end 
end

init_screen
begin
  cbreak
  noecho
  stdscr.keypad(true)
  paddle = Paddle.new

  loop do
    case ch = getch
    when "Q", "q" then break
    when Key::UP, 'U', 'u'then paddle.up
    when Key::DOWN, 'D', 'd' then  paddle.down
    else
      beep
    end
    paddle.draw
  end 
ensure
  close_screen
end

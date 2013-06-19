#!/usr/bin/env ruby2.0

require "trtl"

class Trtl
  def circle
    36.times {
      forward(20)
      turn(10)
    }
  end
end

def dance(turtles,count)
  turtles.each_with_index { |t, i| t.heading = (360/count) * i }
  turtles.pendown
  turtles.circle
end

count = 20
count.times { Trtl.new }
dance(Trtl.world.all,count)
puts "Enter cr to continue"
STDIN.gets 


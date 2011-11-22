#!/usr/bin/env ruby

require 'bindata'

class Point < BinData::Record
  endian :little
  uint16 :x 
  uint16 :y 
end

class Points < BinData::Record
  endian :little
  array  :points, :initial_length => 2 do
    uint8 :x
    uint8 :y
  end
end

# io = File.open("log.bin")
# pt = Points.read(io)
# p pt.snapshot
# pt.points.each { |pt|  puts "x: #{pt.x}, y: #{pt.y}" }

class PointArray < BinData::Array
  endian :little
  default_parameter :initial_length => 128

  point
end

io  = File.open("log.bin")
arr = PointArray.new
arr.read(io)
arr.each { |pt| puts "x: #{pt.x}, y: #{pt.y}" }

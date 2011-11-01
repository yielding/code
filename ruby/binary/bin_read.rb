#!/usr/bin/env ruby

require 'bindata'

class Point < BinData::Record
  endian :little
  uint16 :x 
  uint16 :y 
end

# big endian: 제일 큰 숫자가 저장 장치의 작은 번지에 먼저 쓰여진다.
#             사람이 글을 읽는 방식
class Points < BinData::Record
  endian :little
  array  :points, :initial_length => 128 do
    uint16 :x
    uint16 :y
  end
end

io = File.open("log.bin")
# 1.upto(128) { p  Point.read(io) }
pt = Points.read(io)

pt.points.each { |pt|  puts "x: #{pt.x}, y: #{pt.y}" }

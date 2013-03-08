#!/usr/bin/env ruby1.9
#
require "pp"

image = File.binread("05.bin").unpack("S*")
out   = []
image.each { |color| 
  r = ((color /    1) % 32) * 8; r += r / 32
  g = ((color /   32) % 32) * 8; g += g / 32
  b = ((color / 1024) % 32) * 8; b += b / 32

  out << r << g << b
}

File.binwrite("05.raw", out[0..-1].pack("c*"))

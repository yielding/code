#!/usr/bin/env ruby2.0

require "pp"

unless ARGV.size == 1
  p "usage ./rads.rb filename"
  exit
end

fname    = File.basename(ARGV[0], ".rads")
out_name = "#{fname}.raw"

image = File.binread(ARGV[0]).unpack("S*")
out   = []

image.each { |color| 
  r = ((color /    1) % 32) * 8; r += r / 32
  g = ((color /   32) % 32) * 8; g += g / 32
  b = ((color / 1024) % 32) * 8; b += b / 32

  out << r << g << b
}

File.binwrite(out_name, out[0..-1].pack("c*"))

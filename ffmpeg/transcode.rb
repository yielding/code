#!/usr/bin/env ruby

require "streamio-ffmpeg"

movie = FFMPEG::Movie.new("/Users/yielding/Desktop/IMG_4164.mov")
movie.transcode("movie.mp4") { |progress|
  puts progress
}

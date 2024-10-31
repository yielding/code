#!/usr/bin/env ruby

require "streamio-ffmpeg"

FFMPEG.ffmpeg_binary='/opt/homebrew/bin/ffmpeg'

movie = FFMPEG::Movie.new("/Users/yielding/work/IMG_4164.mov")
movie.transcode("movie.mp4") { |progress|
  puts progress
}

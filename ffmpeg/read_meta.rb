#!/usr/bin/env ruby

require "streamio-ffmpeg"

movie = FFMPEG::Movie.new("/Users/yielding/Desktop/IMG_4164.mov")

p movie.duration # 7.5 (duration of the movie in seconds)
p movie.bitrate # 481 (bitrate in kb/s)
p movie.size # 455546 (filesize in bytes)

p movie.video_stream # "h264, yuv420p, 640x480 [PAR 1:1 DAR 4:3], 371 kb/s, 16.75 fps, 15 tbr, 600 tbn, 1200 tbc" (raw video stream info)
p movie.video_codec # "h264"
p movie.colorspace # "yuv420p"
p movie.resolution # "640x480"
p movie.width # 640 (width of the movie in pixels)
p movie.height # 480 (height of the movie in pixels)
p movie.frame_rate # 16.72 (frames per second)

p movie.audio_stream # "aac, 44100 Hz, stereo, s16, 75 kb/s" (raw audio stream info)
p movie.audio_codec # "aac"
p movie.audio_sample_rate # 44100
p movie.audio_channels # 2

# Multiple audio streams
p movie.audio_streams[0] # "aac, 44100 Hz, stereo, s16, 75 kb/s" (raw audio stream info)

p movie.valid? # true (would be false if ffmpeg fails to read the movie)

movie.screenshot("screenshot.jpg")

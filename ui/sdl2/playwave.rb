#!/usr/bin/env ruby

require 'sdl2'

include SDL2

init(INIT_AUDIO)

#Mixer.init(Mixer::INIT_FLAC|Mixer::INIT_MP3|Mixer::INIT_OGG)
Mixer.open(22050, Mixer::DEFAULT_FORMAT, 2, 512)

p Mixer::Chunk.decoders
p Mixer::Music.decoders
wave = Mixer::Chunk.load(ARGV[0])

Mixer::Channels.fade_in(0, wave, 0, 600)

while Mixer::Channels.play?(0)
  sleep 1
end

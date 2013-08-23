#!/usr/bin/env ruby

begin require 'rubygems'; rescue LoadError; end
require 'rbosa'

itunes = OSA.app('iTunes')
track = itunes.current_track
p track
p trac.name
p trac.artist
p trac.duration
p trac.date_added.to_a
p trac.enabled?

itunes.play

100. times { |i| itunes.sound_volume; sleep 0.1 }

OSA.app('iChat').status_message = "Playing: #{track.name}"

#!/usr/bin/env ruby

require 'pp'
require_relative 'CDJukebox'

#puts $jukebox.seek_time

$jukebox.seek(3, 16) {|x| puts "#{x}% done" } 

#p CDJukebox.new(11223)

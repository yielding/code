#!/usr/bin/env ruby

require "trtl"

t = Trtl.new
20.times { t.left(24); t.forward(30); t.ensure_drawn; }
t.wait

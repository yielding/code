#!/usr/bin/env ruby
# encoding: utf-8

require "fsevent"

class PrintChange < FSEvent
  def on_change(dirs)
    puts "Detected change in: #{dirs.inspect}"
  end

  def start
    puts "watching #{registered_directories.join(", ")} for changes"
    super
  end
end

puts Dir.pwd

printer = PrintChange.new
printer.latency = 0.2
printer.watch_directories %W(#{Dir.pwd} /tmp)
printer.start

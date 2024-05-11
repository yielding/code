#!/usr/bin/env ruby

require "open3"
require 'pathname'

File.open(".ccls", "w") { |file| 
  file.puts "g++-14"
  file.puts "-std=c++2b"
  file.puts "-stdlib=libc++"
  file.puts "-fPIC"
  file.puts ""

  stdout, stderr, status = Open3.capture3("g++-14 -E -x c++ - -v < /dev/null")
  lines = stderr.split("\n")
  start, count = false, 0
  lines.each_with_index { |line, index|
    start = true  if line =~ /#include <...> search starts here:/
    start = false if line =~ /End of search list/
    if start and count > 0
      pn = Pathname.new(line.strip)
      file.puts "-I#{pn.cleanpath.to_s}" 
    end
    count += 1 if start
  }
  file.puts "-I./include" 
  file.puts "-I./develop/include" 
  file.puts "-I./develop/vendor/include" 
  file.puts "-I/opt/homebrew/include/opencv4"
  file.puts "-I/opt/homebrew/include/antlr4-runtime"
}

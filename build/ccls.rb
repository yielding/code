#!/usr/bin/env ruby

require "open3"

File.open(".ccls", "w") { |file| 
  file.puts "g++-11"
  file.puts "-std=c++2a"
  file.puts "-stdlib=libc++"
  file.puts "-fPIC"
  file.puts ""

  stdout, stderr, status = Open3.capture3("g++-11 -E -x c++ - -v < /dev/null")
  lines = stderr.split("\n")
  start, count = false, 0
  lines.each_with_index { |line, index|
    start = true  if line =~ /#include <...> search starts here:/
    start = false if line =~ /End of search list/
    file.puts "-I#{line.strip}" if start and count > 0
    count += 1 if start
  }
}

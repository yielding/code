#!/usr/bin/env ruby

require 'libuv'
require 'libuv/coroutines'

loop = Libuv::Loop.default
loop.run do
  begin
    timer = loop.timer do
      puts "5 seconds passed"
      timer.close
    end
    timer.start(5000)

    # co-routine waits for timer to close
    co timer

    puts "timer handle was closed"
  rescue => error
    puts "error with timer: #{error}"
  end
end

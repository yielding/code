#!/usr/bin/env ruby

require 'libuv'

reactor do |reactor|
  # Perform work on the thread pool with promises
  reactor.work {
    10 * 2
  }.then { |result|
    puts "result using a promise #{result}"
  }

  # Use the coroutine helper to obtain the result without a callback
  result = reactor.work {
    10 * 3
  }.value
  puts "no additional callbacks here #{result}"
end

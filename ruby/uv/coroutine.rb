#!/usr/bin/env ruby

require 'nio'
require 'concurrent'

# NIO4R doesn't have direct coroutine support like libuv,
# but we can use Concurrent Ruby for similar async patterns
selector = NIO::Selector.new

# Using Concurrent Ruby for promise-based async work
pool = Concurrent::FixedThreadPool.new(2)

# Perform work on the thread pool with promises
promise1 = Concurrent::Promise.execute(executor: pool) {
  10 * 2
}.then { |result|
  puts "result using a promise #{result}"
}

# Use future to obtain the result
future = Concurrent::Future.execute(executor: pool) {
  10 * 3
}

# Get the result (blocks until complete)
result = future.value
puts "no additional callbacks here #{result}"

# Wait for promise to complete
promise1.wait

# Cleanup
pool.shutdown
pool.wait_for_termination
selector.close
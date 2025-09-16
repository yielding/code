#!/usr/bin/env ruby

require 'nio'
require 'concurrent'

# NIO4R doesn't have built-in timer support,
# but we can use Concurrent Ruby's ScheduledTask for timers
task = Concurrent::ScheduledTask.execute(5) {
  puts "5 seconds passed"
}

# Wait for the task to complete
task.wait

puts "reactor stopped. No more IO to process"
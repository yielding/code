queue = Dispatch::Queue.new('org.macruby.examples.gcd')
# Synchronously dispatch some work to it.
queue.sync do
  puts 'Starting work!'
  sleep 1.0
  puts 'Done!'
end
# Asynchronously dispatch some work to it.
queue.async do
  puts 'Starting work!'
  sleep 1.0
  puts 'Done!'
end


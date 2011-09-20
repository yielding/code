require 'redis'

r = Redis.new

puts
p 'incr'
r.del 'counter'

p r.incr('counter')
p r.incr('counter')
p r.incr('counter')

puts
p 'decr'
p r.decr('counter')
p r.decr('counter')
p r.decr('counter')

r.expire('counter', 2)
sleep(2)

r.incr('counter')

#!/usr/bin/env ruby

require 'redis'

r = Redis.new

begin
  r.del('foo')

  puts

  p 'set foo to "bar"'
  r.set("foo", "bar")
  p r.get("foo")
rescue => e
  puts "error: #{e.message}"
end

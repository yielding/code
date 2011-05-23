#!/usr/bin/env ruby -wKU

require "redis"

r = Redis.new

r['first_key'] = 'hello world'
puts r['first_key']

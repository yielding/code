#encoding: utf-8

require "redis"

r = Redis.new
r.set("foo", "이창하")
puts r.get("foo")

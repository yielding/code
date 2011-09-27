#!/usr/bin/env ruby -wKU

require "redis"
require "json"

r = Redis.new

r.set "foo", [1, 2, 3].to_json
p JSON.parse(r.get("foo"))

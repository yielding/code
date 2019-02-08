#!/usr/bin/env ruby

require "redis"

class HashSet
  def initialize(host:"localhost", key:"md.image.hashes")
    @redis = Redis.new
    @key   = key
  end

  def clear
    @redis.del(@key)
  end

  def add value
    @redis.sadd(@key, value)
  end

  def remove value
    @redis.srem(@key, value)
  end

  def count_in_cache
    @redis.scard(@key)
  end

  def size
    @redis.scard(@key)
  end

  def members
    @redis.smembers(@key)
  end

  def member? value
    @redis.sismember(@key, value)
  end
end

if __FILE__ == $PROGRAM_NAME
  h = HashSet.new
  h.clear
  #h.add "leech"
  #h.add "leech2"
  #p h.count_in_cache
  #p h.member? "leech"
end

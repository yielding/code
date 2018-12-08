#!/usr/bin/env ruby

require 'msgpack/rpc'
c = MessagePack::RPC::Client.new('127.0.0.1',18800)
result = c.call(:add, 1, 2)  #=> 3
p result

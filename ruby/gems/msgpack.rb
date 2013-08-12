#!/usr/bin/env ruby
#
require "msgpack"
require "pp"

msg = [1, 2, 3].to_msgpack
pp msg

pp MessagePack.unpack(msg)

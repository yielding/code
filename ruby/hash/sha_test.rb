#!/usr/bin/env ruby

require "digest/sha1"

sha = Digest::SHA1.new
sha.update [*0..99].pack("C*")
p sha.hexdigest


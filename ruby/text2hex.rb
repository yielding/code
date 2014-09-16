#!/usr/bin/env ruby

data = File.read("other.bin").split.map { |ch| ch.hex }
File.binwrite("result.bin", data.pack("C*"))

#!/usr/bin/env ruby

data = File.read("other.bin").split.map { |ch| ch.hex }

File.binread("result.bin", data.pack("C*"))
File.binwrite("result.bin", data.pack("C*"))

File.readlines("name").each { |line|

}

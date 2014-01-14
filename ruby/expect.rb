#!/usr/bin/env ruby
# encoding: utf-8

require "pty"
require "expect"

$expect_verbose = true
PTY.spawn("/usr/bin/su - ") do |reader, writer, pid|
  reader.expect("Password:")
  writer.puts "als*****"
  answer = reader.gets

  puts "ok #{answer}"
  writer.puts "/Library/StartupItems/Parallels/Parallels stop"
  reader.expect("kextunload: unload kext /System/Library/Extensions/hypervisor.kext succeeded")
end

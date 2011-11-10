#!/usr/bin/env ruby -wKU

`sudo /usr/sbin/bless --device /dev/disk0s4 --setBoot --legacy --nextonly; sudo shutdown -r now`
# require "pty"
# require "expect"

# $expect_verbose = true
# cmd = "sudo /usr/sbin/bless --device /dev/disk0s4 --setBoot --legacy --nextonly"
# PTY.spawn(cmd) do |r, w, pid|
#   r.expect("Password:") { w.puts("alsrudk!") }
# end

# cmd = "sudo shutdown -r now"
# PTY.spawn(cmd) do |reader, writer, pid|
#   reader.expect("Password:")
#   writer.puts "alsrudk!"
# end

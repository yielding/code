#!/usr/bin/env ruby
 
require 'socket'

tester = UDPSocket.new
tester.connect("127.0.0.1", 1617)
tester.send(ARGV[0], 0)
puts tester.recvfrom(100)[0]

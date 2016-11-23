#!/usr/bin/env ruby

require 'socket'

MULTICAST_ADDR = "225.4.5.6" 
PORT = 5000

begin
  socket = UDPSocket.open
  socket.setsockopt(Socket::IPPROTO_IP, Socket::IP_TTL, [1].pack('i'))
  socket.send(ARGV.join(' '), 0, MULTICAST_ADDR, PORT)
ensure
  socket.close 
end

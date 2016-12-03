#!/usr/bin/env ruby

require 'socket'

MULTICAST_ADDR = "239.0.0.222"
PORT = 2222

begin
  socket = UDPSocket.open
  socket.setsockopt(Socket::IPPROTO_IP, Socket::IP_TTL, [128].pack('i'))
  socket.send(ARGV.join(' '), 0, MULTICAST_ADDR, PORT)

  #1.upto(100) { |no| socket.send(no.to_s, 0, MULTICAST_ADDR, PORT) }
ensure
  socket.close 
end

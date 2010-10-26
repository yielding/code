#!/usr/bin/env ruby

require 'socket'

logger = UDPSocket.new
logger.bind("127.0.0.1", 1617)
loop do
  puts "message here!!!"
  msg =  logger.recvfrom(1000)[0].chomp
  puts msg
  logger.send(msg, 0)
end

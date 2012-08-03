#!/usr/bin/env ruby

require 'socket'
require_relative 'byte_buffer'

puts "server start...\n"

server = TCPServer::new('127.0.0.1', 7780)
begin
  while (session = server.accept)
    while true do
      res = session.recv(37)
      pdu = ByteBuffer.new
      pdu.from_packet(res)

      pdu.get_int1 # 0xf0
      pdu.get_int1 # 0xf0

      cmd = pdu.get_int1
      len = pdu.get_int2

      pdu.get_int2 # rsv dummy 
      pdu.get_int1 # pid dummy

      puts pdu.get_string(len -3)

      pdu.get_int1 #0xfe
      pdu.get_int1 #0xfe

      out = ByteBuffer.new(10)
      out.set_int1(0xf0).set_int1(0xf0)
      out.set_int1(cmd + 0x80).set_int1(0).set_int1(3).set_int1(0).set_int1(0).set_int1(0)
      out.set_int1(0xfe).set_int1(0xfe)

      session.send(out.to_packet, 0)

    end
    session.close
  end
rescue
  puts "Error: #{$!}"
ensure
  server.close
end

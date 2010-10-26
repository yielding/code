#!/usr/bin/env ruby

require 'socket'
require 'byte_buffer'
require 'pp'

puts "server start...\n"

port = 12346
server = TCPServer::new('localhost', port)
begin
  while (session = server.accept)
    # puts "data #{session.gets}"
    res = session.recv(8)
    # pdu = ByteBuffer.new(res.unpack("C*").to_a)
    pdu = ByteBuffer.new
    pdu.from_packet(res)
    pp pdu.get_int4
    pp pdu.get_int1
    pp pdu.get_int1
    pp pdu.get_int1
    pp pdu.get_int1
    session.close
  end
rescue
  puts "Error: #{$!}"
ensure
  server.close
end

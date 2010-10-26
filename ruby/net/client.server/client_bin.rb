#!/usr/bin/env ruby

require 'socket'
require 'byte_buffer'
require 'timeout'

port   = 12346
client = nil
begin
  timeout(1) do
    client = TCPSocket::new('localhost', port)
  end
rescue
  puts "Timeout error: #{$!}";
else

  pdu = ByteBuffer.new
  pdu.set_int4(100)
  pdu.set_int1(0)
  pdu.set_int1(1)
  pdu.set_int1(2)
  pdu.set_int1(3)

  sent = client.send(pdu.to_packet, 0)
ensure
  client.close
end

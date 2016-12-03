#!/usr/bin/env ruby

require 'libuv'

reactor do |reactor|
  begin
    reactor.tcp { |data, socket|
      puts "received: #{data}"
      socket.close
    }
    .connect('127.0.0.1', 3000)
    .start_read
    .write("GET / HTTP/1.1\r\n\r\n")
  rescue => error
    puts "error: #{error}"
  end
end


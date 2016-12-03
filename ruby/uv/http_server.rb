#!/usr/bin/env ruby
require 'libuv'

reactor do |reactor|
  reactor.tcp { |data, socket|
    puts "received: #{data}"
    socket.close
  }
  .connect('127.0.0.1', 3000) { |socket|
     socket.start_read
           .write("GET / HTTP/1.1\r\n\r\n")
  }
  .catch { |error|
    puts "error: #{error}"
  }
  .finally {
    puts "socket closed"
  }
end

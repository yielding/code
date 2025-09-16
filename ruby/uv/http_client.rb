#!/usr/bin/env ruby

require 'nio'
require 'socket'

selector = NIO::Selector.new

begin
  # Create TCP socket
  socket = TCPSocket.new('127.0.0.1', 3000)

  # Register socket with selector for read/write events
  monitor = selector.register(socket, :w)

  # Select for write readiness
  selector.select(5) do |monitor|
    if monitor.writable?
      # Send HTTP request
      socket.write("GET / HTTP/1.1\r\nHost: localhost\r\n\r\n")

      # Switch to reading
      monitor.interests = :r
    end
  end

  # Select for read readiness
  selector.select(5) do |monitor|
    if monitor.readable?
      # Read response
      data = socket.read_nonblock(4096, exception: false)
      unless data == :wait_readable
        puts "received: #{data}"
      end
    end
  end

rescue => error
  puts "error: #{error}"
ensure
  selector.deregister(socket) if socket
  socket.close if socket
  selector.close
end
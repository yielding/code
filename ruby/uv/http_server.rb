#!/usr/bin/env ruby

require 'nio'
require 'socket'

selector = NIO::Selector.new

begin
  # Create server socket
  server = TCPServer.new('127.0.0.1', 3000)
  selector.register(server, :r)

  puts "Server listening on 127.0.0.1:3000"

  loop do
    # Wait for events
    selector.select do |monitor|
      case monitor.io
      when TCPServer
        # Accept new client
        client = monitor.io.accept_nonblock(exception: false)
        unless client == :wait_readable
          puts "Client connected"
          selector.register(client, :r)
        end

      when TCPSocket
        # Handle client data
        begin
          data = monitor.io.read_nonblock(4096, exception: false)
          if data == :wait_readable
            next
          elsif data.nil? || data.empty?
            # Client disconnected
            puts "socket closed"
            selector.deregister(monitor.io)
            monitor.io.close
          else
            puts "received: #{data}"
            # Echo back a simple HTTP response
            response = "HTTP/1.1 200 OK\r\nContent-Length: 13\r\n\r\nHello, World!"
            monitor.io.write(response)
            selector.deregister(monitor.io)
            monitor.io.close
          end
        rescue EOFError
          puts "socket closed"
          selector.deregister(monitor.io)
          monitor.io.close
        end
      end
    end
  end

rescue Interrupt
  puts "\nShutting down server..."
rescue => error
  puts "error: #{error}"
ensure
  selector.close
  server.close if server
end
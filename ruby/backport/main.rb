#!/usr/bin/env ruby

require 'backport'

module MyAdapter
  def opening
    puts "Opening a connection"
  end

  def closing
    puts "Closing a connection"
  end

  def receiving data
    write "Client sent: #{data}"
  end
end

Backport.run do
  Backport.prepare_tcp_server(host: 'localhost', port: 8000, adapter: MyAdapter)
end
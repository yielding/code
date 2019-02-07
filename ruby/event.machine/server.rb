#!/usr/bin/env ruby

require 'eventmachine'

class EchoServer < EM::Connection
  def receive_data(data)
    send_data(data)
  end
end

EventMachine.run do
  Signal.trap("INT")  { EventMachine.stop }
  Signal.trap("TERM") { EventMachine.stop }

  EventMachine.start_server("0.0.0.0", 10000, EchoServer)
end

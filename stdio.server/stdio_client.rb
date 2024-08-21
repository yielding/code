#!/usr/bin/env ruby

require 'json'

def send_request(data)
  puts data.to_json
  STDOUT.flush
  response = gets
  JSON.parse(response)
end

request = { message: "Hello, Server!" }
response = send_request(request)
puts "Server response: #{response['response']}"
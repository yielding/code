#!/usr/bin/env ruby

require 'json'

def process_request(request)
  # Example processing: Echoing back the received request
  { response: "Received: #{request['message']}" }
end

def main
  ARGF.each_line do |line|
    request = JSON.parse(line.strip)
    response = process_request(request)
    puts response.to_json
    STDOUT.flush  # Ensure the output is flushed
  end
end

main if __FILE__ == $0
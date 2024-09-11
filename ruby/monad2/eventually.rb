#!/usr/bin/env ruby
# continuation monad

require 'monads/eventually'

include Monads

eventaul_str = Eventually.new do |success|
  Thread.new do
    sleep 2
    p "1"
    success.call("hello world")
  end
end

eventaul_res = eventaul_str.and_then do |string|
  Eventually.new do |success|
    Thread.new do
      sleep 3
      p "2"
      success.call(string.upcase)
    end
  end
end

eventaul_res.run { |string| puts string }

sleep 10
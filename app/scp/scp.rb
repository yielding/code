#!/usr/bin/env ruby

require 'net/scp'

Net::SCP.start("gmd_server", "yielding", :password => "alsrudk!") do |scp|
  scp.download!("test", ".", :recursive => true)
end

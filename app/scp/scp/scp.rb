#!/usr/bin/env ruby

require 'net/scp'

id = "yielding"
pw = "alsrudk!"
Net::SCP.start("127.0.0.1", id, :password => pw) { |scp|
  scp.download!("test", ".", :recursive => true)
}

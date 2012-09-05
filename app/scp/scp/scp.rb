#!/usr/bin/env ruby19

require 'net/scp'

id = "root"
pw = "alpine"
Net::SCP.start("127.0.0.1", id, :password => pw, :port => 2222) { |scp|
  scp.download!("/mnt2/mobile/Library", "/Users/yielding/down", 
                :recursive => true, 
                :verbose   => true)
}

#!/usr/bin/env ruby

require 'net/scp'

id = "root"
pw = ""
Net::SCP.start("127.0.0.1", id, :password => pw, :port => 2222) { |scp|
  scp.download!("/mnt2", "/Users/yielding/down", 
                :recursive => true, 
                :verbose   => true)
}

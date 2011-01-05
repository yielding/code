#!/usr/bin/env ruby -KU

require "net/ssh"

HOST = "172.30.1.1"
USER = 'yielding'
PASS = 'alsrudk!'

Net::SSH.start(HOST, USER, :password => PASS) do |ssh|
  result = ssh.exec!('ls -al')
  puts result
end

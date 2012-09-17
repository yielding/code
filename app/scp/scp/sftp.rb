#!/usr/bin/env ruby19

require "net/sftp"

host = '127.0.0.1'
user = 'root'
pw   = 'alpine'

Net::SFTP.start(host, user, :password => pw, :port=>2222) { |sftp|
  sftp.download!("/mnt2/mobile/Media", "/Users/yielding/down", 
                :recursive => true, 
                :verbose   => true)
}

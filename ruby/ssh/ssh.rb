#!/usr/bin/env ruby -wKU
require "net/ssh"

cmd = "dd if=/dev/zero bs=1M count=1"
cmd = "cat ./dump_data_partition.sh"
Net::SSH.start('localhost', "root", :password=> "", :port=>2222) do |ssh|
  size = 0
  ssh.exec(cmd) do |channel, stream, data|
    if stream == :stdout
      size += data.size
      STDERR << size / 1024 << " Kbytes\n"
      print data 
    end
  end
end

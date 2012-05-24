#!/usr/bin/env ruby

require "connection"
require "pp"

conn = RFIDKTReaderConnection.new(1, "rfid")
puts "open fail" unless conn.open("127.0.0.1", 7780, 1000)
puts "open ok"

while true do
  1.upto(10) do |tid|
    pid = 10
    cmd =  3
    tid = sprintf("%08d", tid)
    sdu = "EB00001|KR000010|#{tid}|0"
    conn.execute(cmd, pid, sdu)
    conn.recv(cmd)
    puts "recv ok #{tid}"
    sleep 1
  end
end

conn.close

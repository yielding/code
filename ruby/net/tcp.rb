require 'socket'

client = TCPSocket.new('192.168.10.113', 8091)
data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
client.send(data.pack("C*"), 0)

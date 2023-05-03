#!/usr/bin/env python

import socket
import sys

HOST, PORT = "localhost", 9999
data = " ".join(sys.argv[1:])

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
try:
  sock.connect((HOST, PORT))
  sock.send(data + "\n")

  received = sock.recv(1024)
finally:
  sock.close()

print "Sent:     {}".format(data)
print "Received: {}".format(received)

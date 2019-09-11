#!/usr/bin/env python
# -*- coding: utf-8 -*-

import zmq
import random
import sys
import time

port = "5556"
context = zmq.Context()
socket = context.socket(zmq.PAIR)
socket.connect("tcp://localhost:%s" % port)

while True:
    socket.send("Server message to client3")
    msg = socket.recv()
    print(msg)
    socket.send("client message to server1")
    socket.send("client message to server2")
    time.sleep(1)

#!/usr/bin/env python
# -*- coding: utf-8 -*-

import zmq
import random
import sys
import time

port = "5555"
context = zmq.Context()
socket = context.socket(zmq.REQ)
socket.connect("tcp://localhost:%s" % port)

while True:
    socket.send("Server message to client3")
    msg = socket.recv()
    print(msg)
    time.sleep(1)

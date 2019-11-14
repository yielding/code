#!/usr/bin/env python
# -*- coding: utf-8 -*-

import zmq
import random
import sys
import time

port = "5556"
context = zmq.Context()
socket = context.socket(zmq.PAIR)
socket.bind("tcp://*:%s" % port)

while True:
    socket.send("server message to client3")
    msg = socket.recv()
    print(msg)
    time.sleep(1)

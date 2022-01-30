#!/usr/bin/env node

const express = require("express")
const app = express();

app.use("/", function(req, res) {
  res.sendFile(__dirname + '/index.html')
});

app.listen(8080);

const WebSocket = require('ws')

const socket = new WebSocket.Server({
  port: 8081
});

socket.on('connection', function(ws, req) {
  ws.on('message', (msg) => {
    console.log('유저 메시지 : ' + msg)
  })
});

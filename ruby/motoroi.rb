#!/usr/bin/env ruby 

require 'net/http'
require 'json'
require 'pp'

httpclient = Net::HTTP::new("192.168.1.7", 8080)
resp, data = httpclient.get("/personalportal/sms?c=0&d=4&blk=1")
dict = JSON.parse(data)
p dict["LoadSMSListResp"]["MsgDesc"][1]["Name"]

#!/usr/bin/env ruby 

require 'net/http'
require 'json'
require 'pp'

httpclient = Net::HTTP::new("192.168.1.7", 8080)
resp, data = httpclient.get("/personalportal/sms?c=0&d=4&blk=1")
dict = JSON.parse(data)
p dict["LoadSMSListResp"]["MsgDesc"][1]["Name"]

# resp, data = httpclient.get("/personalportal/pictureview?c=0&blk=1")
# p resp
# pp JSON.parse(data)

picture = "/personalportal/pictureview?c=5&id=9b969e6b-1aec-466f-af26-bf6f98c0c146"
resp, data = httpclient.get(picture)
# p data
# File.open("haeum.jpg") do { |f|
#   f.write(data)
# }


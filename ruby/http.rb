#!/usr/bin/env ruby

require 'net/http'
require 'pp'

httpclient = Net::HTTP::new("www.chosun.com")
resp, data = httpclient.get("/index.html")
print data
pp resp

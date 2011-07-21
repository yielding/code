#!/usr/bin/env ruby 

require "net/http"
require "pp"

http = Net::HTTP::new("192.168.1.7", 8080)
_, data = http.get("/personalportal/pictureview?c=5&id=f9f3ca42-0c31-49fc-9d07-8367e133f7f7")
File.open("y.jpg", "wb") { |file| file.write(data) }


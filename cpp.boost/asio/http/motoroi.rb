#!/usr/bin/env ruby 

require "net/http"

http = Net::HTTP::new("192.168.1.7", 8080)
_, data = http.get("/personalportal/mobileinbox?c=0&d=4&blk=1")

# http = Net::HTTP::new("www.forumgarden.com", 80)
# _, data = http.get("/forums/attachments/current-events/34561d1280425002-bear-attacks-near-yellowstone-soda-butte-campground-bear-attack.jpg");
File.open("y.jpg", "wb") { |file| file.write(data) }


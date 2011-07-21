#!/usr/bin/env ruby 

require "net/http"

http = Net::HTTP::new("172.30.1.2", 8080)
_, data = http.get("/personalportal/pictureview?c=5&id=f9f3ca42-0c31-49fc-9d07-8367e133f7f7")

# http = Net::HTTP::new("www.forumgarden.com", 80)
# _, data = http.get("/forums/attachments/current-events/34561d1280425002-bear-attacks-near-yellowstone-soda-butte-campground-bear-attack.jpg");
File.open("y.jpg", "wb") { |file| file.write(data) }


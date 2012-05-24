#!/usr/bin/env ruby -wKU

image = File.binread("1.jpg")
b = (image[1..-1] =~ /\xFF\xD8\xFF/n) + 1
e = (image =~ /\xFF\xD9/n)
File.binwrite("1_tmb.jpg", image[s, e - b])

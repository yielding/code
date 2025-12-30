#!/usr/bin/env ruby

image = File.binread("../data/taekwon_v.jpg")

b = (image[1..-1] =~ /\xFF\xD8\xFF/n) + 1
e = (image =~ /\xFF\xD9/n)

File.binwrite("1_thumb.jpg", image[b, e - b])

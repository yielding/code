#!/usr/bin/env ruby -wKU
#-*- coding:utf-8 -*-

require "json"

dict  = JSON.parse(File.read("dictionary.json"))
files = [ "StartPage.xaml", "bin/StartPage.xaml" ]

files.each { |file|  
  text = File.read(file)
  dict.each { |word| 
    key, eng = word[0], word[1][0] 
    text.gsub!(key, eng)
  }

  File.open("#{file}_out", "w") { |f| f.write(text) }
}

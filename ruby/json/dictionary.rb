#!/usr/bin/env ruby -wKU
#-*- coding:utf-8 -*-

require "json"

files = [ "StartPage.xaml", "bin/StartPage.xaml" ]
text  = File.read("dictionary.json")
dict  = JSON.parse(text).sort_by { |d| d[0].length * -1 }

files.each { |file|  
  text = File.read(file)
  dict.each { |word| 
    key, eng = word[0], word[1][0] 
    text.gsub!(key, eng)
  }

  File.open("#{file}_out", "w") { |f| f.write(text) }
}

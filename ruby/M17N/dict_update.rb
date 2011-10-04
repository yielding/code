#!/usr/bin/env ruby -KU
#-*- coding:utf-8 -*-

require_relative "dict_library"

def update_dictionary dict_file, resource_files
  return nil unless File.exist?(dict_file)

  dict = MD::Dictionary.new(dict_file)
  resource_files.each { |file| 
    f = MD::ResourceFile.factory(file)
    dict.update(f.extract_key)
  }

  dict
end

data   = JSON.parse(File.read("dict_data.json"))
files1 = data["xaml"].merge(data["xml"]).merge(data["rc"])
files2 = data["setting"]

puts " ===== kje_dictionary.json contents ===== "
d1 = update_dictionary("kje_dictionary.json", files1)
d1.print
d1.save unless ARGV.include? "-p" 

puts " ===== settings.json contents ===== "
d2 = update_dictionary("settings.json", files2)
d2.print
d2.save unless ARGV.include? "-p" 

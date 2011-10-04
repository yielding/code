#!/usr/bin/env ruby -KU
#-*- coding:utf-8 -*-

require_relative "dict_library"

def translate(files, dict)
  files.each { |file|
    f = MD::ResourceFile.factory(file)
    f.save_eng(f.with(dict).to_english)
    f.save_jpn(f.with(dict).to_japanese)
  }
end

data   = JSON.parse(File.read("dict_data.json"))
files1 = data["xaml"].merge(data["xml"]).merge(data["rc"])
files2 = data["setting"]

translate(files1, MD::Dictionary.new("kje_dictionary.json"))
translate(files2, MD::Dictionary.new("settings.json"))

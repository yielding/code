#!/usr/bin/env ruby -wKU
# encoding: utf-8

require 'fast-aes'

key = '42#3b%c$dxyT,7a5=+5fUI3fa7352&^:'
aes = FastAES.new(key)

text = "Hey there, how are you?"
data = aes.encrypt(text)
p data
p aes.decrypt(data)

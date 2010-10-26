#!/usr/bin/env ruby1.9
# -*- encoding: utf-8 -*-

require 'json'

#data = { name: 'leech', address: ['tx', 'ty'], age:227 }
data = { name: '이창하', address: [ 'tx', 'usa' ], age: 17 } 


serialized = data.to_json
puts serialized
File.open('dat.json', "w") { |f| f.puts serialized }

#!/usr/bin/env ruby -KU
#encoding: utf-8

require "openssl"

cipher = OpenSSL::Cipher::Cipher.new("AES-256-CBC")

key = cipher.random_key()
iv  = cipher.random_iv()

puts "key: #{key}, iv: #{iv}"


#---- Encrypt
text = "Hello, World!"
cipher.encrypt
cipher.key = key
cipher.iv  = iv
e = cipher.update(text) << cipher.final
puts("Encrypted text: " + e.to_s())


#---- Decrypt
cipher = OpenSSL::Cipher::Cipher.new("AES-256-CBC")
cipher.decrypt
cipher.key = key
cipher.iv = iv
d = cipher.update(e) << cipher.final

puts("Plain text: " + d.to_s())

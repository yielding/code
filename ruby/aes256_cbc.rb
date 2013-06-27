#!/usr/bin/env ruby
#encoding: utf-8

require "openssl"
require "base64"

cipher = OpenSSL::Cipher::Cipher.new("AES-256-CBC")

key = cipher.random_key()
iv  = cipher.random_iv()

#puts "key: #{key}, iv: #{iv}"

#---- Encrypt
text = "Hello, World!"
cipher.encrypt
cipher.key = key
cipher.iv  = iv
e = cipher.update(text) << cipher.final
encrypted = Base64.encode64(e.to_s())

puts("Encrypted text: " + Base64.encode64(e.to_s()))

#---- Decrypt
cipher = OpenSSL::Cipher::Cipher.new("AES-256-CBC")
cipher.decrypt
cipher.key = key
cipher.iv  = iv
e = Base64.decode64(encrypted)
d = cipher.update(e) << cipher.final

puts "Plain text:#{d}"

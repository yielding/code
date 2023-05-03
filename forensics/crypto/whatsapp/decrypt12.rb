#!/usr/bin/env ruby

require 'openssl'

def read_keyfile(path)
  return nil unless File.size?(path) == 158
  File.binread(path, 32, 0x7e)
end

def read_iv(path)
  File.binread(path, 16, 0x33)
end

def decrypt12(path, key, iv, of)
  len  = File.size?(path)
  data = File.binread(path, len - 87, 0x43)

  cipher = OpenSSL::Cipher.new("aes-256-gcm")
  cipher.decrypt 
  cipher.key = key
  cipher.iv  = iv

  cipher.update(data) + cipher.final
end

key  = "key"
path = "msgstore.db.crypt12"
key  = read_keyfile(key)
iv   = read_iv(path)

decrypt12(path, key, iv, "res.db")

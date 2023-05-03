#!/usr/bin/env ruby

require 'openssl'
require 'digest/sha2'
require "base64"
require 'pp'

=begin
d = OpenSSL::Cipher.new('AES-256-CBC')
d.decrypt
d.padding = 0

key = OpenSSL::PKCS5.pbkdf2_hmac_sha1(
	File.binread("c:/Users/yielding/Desktop/passwd_hash.bin"),
	File.binread("c:/Users/yielding/Desktop/salt.bin"),
	10000,
	d.key_len)

d.key = key

ciphertext = Base64.decode64(File.binread("c:/Users/yielding/Desktop/bplist.bin"))
res = d.update(ciphertext) << d.final
File.binwrite("c:/Users/yielding/Desktop/res.bin", res)
puts res
=end

#=begin
ciphertext = Base64.decode64(File.binread("c:/Users/yielding/Desktop/bplist.bin"))
salt = File.binread("c:/Users/yielding/Desktop/salt.bin")
key = File.binread("c:/Users/yielding/Desktop/passwd_hash.bin")
#key = OpenSSL::PKCS5.pbkdf2_hmac_sha1(key, salt, 10000, cipher.key_len)

cipher = OpenSSL::Cipher.new('AES-256-CBC')
cipher.decrypt
cipher.padding = 0
cipher.key = key
#cipher.iv = "\x00"*10 + salt

res = cipher.update(ciphertext) << cipher.final

File.binwrite("c:/Users/yielding/Desktop/res.bin", res)
puts res
#=end

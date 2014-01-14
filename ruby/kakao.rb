#!/usr/bin/env ruby
# encoding: utf-8

require 'openssl'
require "base64"
require "pp"

d = OpenSSL::Cipher.new("AES-256-CBC")
d.decrypt
#d.encrypt

password = [22, 8, 9, 111, 2, 23, 43, 8, 33, 33, 10, 16, 3, 3, 7, 6].pack('n*')
salt     = [50, 51, 51, 48, 51, 51, 55, 48, 0, 0, 0, 0, 0, 0, 0, 0].pack('c*')
p "password: #{password}, size: #{password.size}"
p "salt    : #{salt}, size: #{salt.size}"

key = OpenSSL::PKCS5.pbkdf2_hmac_sha1(
      password, 
      salt,
      2,          # iteratoin
      d.key_len)  # key_len(bytes)

d.key = key

#d.iv = "ecbc985b3550edc977a17acc066f2192".scan(/../).map { |b|
        #b.hex
        #}.pack('c*')

d.iv = [15, 8, 1, 0, 25, 71, 37, -36, 21, -11, 23, -32, -31, 21, 12, 53].pack('c*')
p "iv === #{iv}"

encrypted = Base64.decode64("m+oavcl6PVEo1RBcCFlKSQ==")
p "decoded: #{encrypted}"

#data  = "4a39f1a967c728e11c7a5a3fb5d73ad07561f504c9d084d0b1ae600cc1f75137cbb82a4d826c060cb06e2e283449738d".scan(/../).map{ |b|
          #b.hex
        #}.pack('c*')

p d.update(encrypted) << d.final
#res = (d.update("01027707344") << d.final)


#!/usr/bin/env ruby
# encoding: utf-8

require "base64"
require 'pp'

class String
  def self_closing?
    self =~ /value/ 
  end
end

res = []
File.readlines("dec-email.xml").each do |line|
  next unless line =~ /^ /
  pattern = line.self_closing? ? /.+<(.+?) name="(.+?)" value="(.+?)"/
                               : /.+<(.+?) name="(.+?)">(.*?)<\/.+/
  if result = line.match(pattern)
    type, key, value = result.captures
    value = Base64.decode64(value) if type == "string" and value.end_with?("==")
    key = key.split(".").last
    res << [key, value]
  end
end

[*1..res.length].zip(res).each { |e| 
  puts sprintf("%-3d: %s" , e[0] , e[1].join(".")) 
}

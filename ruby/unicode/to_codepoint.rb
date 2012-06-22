#!/usr/bin/env ruby -wKU
# encoding: utf-8

def to_codepoint(ch)
  puts "0x#{ch.codepoints.first.to_s(16)}"
end

to_codepoint(",")

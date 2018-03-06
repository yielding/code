#!/usr/bin/env ruby

require 'pp'

class String
  def to_codepoint
    chars = self
    chars.bytes.each_slice(3).map { |s| 
      first = (s[0] & 0xf0) >> 4
      return [] unless first == 0b1110

      res = s[0] & 0x0f
      res = res << 6 | s[1] & 0x3f
      res = res << 6 | s[2] & 0x3f
    }
  end
end

def read_utf8_from path
  File.open(path, "r:UTF-8") do |f| 
    f.read.chomp
  end
end


if __FILE__ == $PROGRAM_NAME
  path = if ARGV.length != 1
           "yielding.txt"  
         else
           ARGV[1]
         end

  chars = read_utf8_from(path)
  chars.to_codepoint.each { |cp|
    p "U+#{cp.to_s(16)}"
  }
end

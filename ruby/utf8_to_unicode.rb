#!/usr/bin/env ruby

class String
  def to_codepoint
    self.bytes.each_slice(3).map do |s| 
      first = (s[0] & 0xf0) >> 4
      return nil unless first == 0b1110

      first = s.shift & 0x0f
      s.reduce(first) { |result, e| result << 6 | e & 0x3f }
    end
  end
end

chars = File.open("yielding.txt", "r:UTF-8") { |f| f.read.chomp }
chars.to_codepoint.each do |cp|
  p "U+#{cp.to_s(16).upcase}"
end

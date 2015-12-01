#!/usr/bin/env ruby
# encoding: utf-8

require_relative "phoneme"
require "pp"

=begin rdoc
  1. 펼쳐진 모음을 모은다.
=end

class Numeric
  def to_hex; "0x#{self.to_s(16)}" end
  def to_bin; "0b#{self.to_s(2)}"  end
end

class Array
  def to_hex; "[" + self.map { |e| e.to_hex }.join(", ") + "]" end
end


class Korean
  def initialize()
    @first   = 0x1100..0x1112
    @center  = 0x1161..0x1175
    @last    = 0x11a8..0x11c2
    @compat  = 0x3130..0x318e
    @phoneme = Phoneme.new
  end

  def transcode(s)
    res = ""
    i = 0
    while i < s.length
      ch = 0
      if @first.include?(s[i])
        puts "if i #{i}"
        use_count = 2
        first  = s[i] - @first.first
        second = s[i + 1] - @center.first
        thrid  = 0
        if i + 2 < s.length and @last.include?(s[i+2])
          thrid = s[i+2] - @last.first + 1
          use_count += 1
        end

        ch = (first * 21 + second) * 28 + thrid + 0xac00
        i += use_count
      else
        puts "else i #{i}: #{s[i].to_s(16)}"
        ch = s[i]
        i += 1
      end

      res += [ch].pack("U")
    end

    res
  end

  def transcode_compat(codepoints)
    p codepoints.to_hex

    vowel_combined = self.combine_duped_vowels(codepoints)
    p vowel_combined.to_hex

    default_jamo = vowel_combined.map { |cp| self.from_compatible_jamo(cp) }
    p default_jamo.to_hex

    return self.transcode(default_jamo)
  end

  def combine_duped_vowels(codes)
    result = []
    skip = false
    codes.each_cons(2) do |c|
      if skip
        skip = false
        next
      end

      if c == [0x3157, 0x3163]   # ㅗ ㅣ
        result.push(0x315A)      # 외
        skip = true 
      else
        result.push(c[0])
      end
    end

    result.push(codes.last)
    result
  end

  def from_compatible_jamo(code)
  end
end

# ok
f = File.open("keyboard.dat", "r:utf-8:utf-16le")
f.pos = 0x18

# ok
data = f.read()
sentences = data.split "\u0000".encode("utf-16le")
ss = sentences[1]
pp ss

# ok
codepoint = ss.codepoints.map { |c| c }
#pp codepoint.to_hex
#pp codepoint.pack("U*").unicode_normalize(:nfkc)
 
kor = Korean.new
p kor.transcode_compat(codepoint)
 
# 안되겠다

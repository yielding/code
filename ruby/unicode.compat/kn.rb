#!/usr/bin/env ruby
# encoding: utf-8

require_relative "phoneme"
require "pp"

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
        ch = s[i]
        i += 1
      end

      res += [ch].pack("U")
    end

    res
  end

  def transcode_compat(codepoints)
    f0 = @phoneme.compact_vowels(codepoints)
    f1 = @phoneme.compact_cons(f0)
    default_jamo = @phoneme.to_jamo(f1)

    return self.transcode(default_jamo)
  end
end

# ok
f = File.open("keyboard.dat", "r:utf-8:utf-16le")
f.pos = 0x18

# ok
data = f.read()
sentences = data.split "\u0000".encode("utf-16le")

kor = Korean.new
sentences.each { |s|
  next if s.length < 2
  codepoints = s.codepoints.map { |c| c }
  pp codepoints.to_hex
  pp codepoints.pack("U*").unicode_normalize(:nfd)
  pp kor.transcode_compat(codepoints)
}

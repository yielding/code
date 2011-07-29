#!/usr/bin/env ruby -wKU
#encoding: utf-8

class String
  @@beg, @@mid, @@end = [*0x1100..0x1112], [*0x1161..0x1175], [0, *0x11A8..0x11C2]

  def separate
    separated = []
    self.unpack("U*").each do |c|
      if c.between?(0xAC00, 0xD7A3) # 유니코드 2.0 한글의 범위 
        pos = c - 0xAC00
        n1  = pos / (21 * 28)  # 초성 : ‘가’ ~ ‘깋’ -> ‘ㄱ’
        pos = pos % (21 * 28)  # ‘가’ ~ ‘깋’에서의 순서
        n2  = pos / 28;        # 중성
        n3  = pos % 28;        # 종성

        separated << @@beg[n1] << @@mid[n2] 
        separated << @@end[n3] if (n3 > 0)
      else
        separated << c
      end
    end
    separated
  end

  def korean?
    self.unpack("U*").any? { |c| c.between?(0xAC00,0xD7A3) or c.between?(0x3131,0x318E) }
  end
end

a = "민경아"; 
if a.korean?
  b = a.separate
  p b
  p b.reduce([]) { |r, e| r << [e].pack("U*") }
  p b.pack("U*")
end

a = "abc"
b = a.separate
p b.pack("U*")

puts ""
p "abc".korean?
p "abㄱ".korean?
p "ab가".korean?


#!/usr/bin/env ruby -wKU

#encoding: utf-8

require "test/unit"

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
    self.unpack("U*").any? { |c| in_kor_range(c) }
  end

  def extract_kor
    b = -1
    b, e = self.index(/\p{Hangul}/), self.rindex(/\p{Hangul}/)
    return (b > -1) ? self[b, e - b + 1] : ""
  end

  def in_kor_range(c)
    c.between?(0xAC00,0xD7A3) or c.between?(0x3131,0x318E)
  end

end

class TestKoreanString < Test::Unit::TestCase
  def setup
    @kamin = "민경아"
  end

  def test_separate
    sep = [4358, 4469, 4523, 4352, 4455, 4540, 4363, 4449]
    assert_equal(@kamin.separate, sep)
  end

  def test_korean_consonant_vowel
    a = ["ᄆ", "ᅵ", "ᆫ", "ᄀ", "ᅧ", "ᆼ", "ᄋ", "ᅡ"] 
    b = @kamin.separate.reduce([]) { |r, e| r << [e].pack("U*") }
    assert_equal(a,b)
  end

  def test_eng
    a = "abc"
    b = a.separate
    assert_equal("abc", b.pack("U*"))
  end

  def test_test
    assert_equal("abc".korean?, false)
    assert_equal("abㄱ".korean?, true)
    assert_equal("ab가".korean?, true)
  end

  def test_extract_korean
    line = "<Log name=\"ExportErrorMaxPath\" level=\"error\" title=\"내보내기 할 파일의 경로가 너무 깁니다.\">"
    kor  = "내보내기 할 파일의 경로가 너무 깁니다"
    assert_equal(kor, line.extract_kor)
  end

  def test_ok
    a = "ㅇㅇㅇ"
    puts a.korean?

  end
end

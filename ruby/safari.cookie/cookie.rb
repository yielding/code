#!/usr/bin/env ruby

require 'pp'

class Numeric
  def to_hex; "0x#{self.to_s(16)}" end
end

class String
  def get_int4_be; self.unpack("N")[0] end
  def get_int4_le; self.unpack("l")[0] end
end

class Cookie
end

class Page
  attr_reader :ok

  def initialize(f, pos)
    f.seek(pos)
    @ok = f.read(4).get_int4_be == 256
  end

  def to_s
    "#{@pos.to_s(16)}"
  end
end

class SafariCookie
  attr_reader :ok, :page_count
  attr_reader :page_offset

  def initialize path 
    @offsets = []
    @f  = File.open path 
    @ok = @f.read(4) == "cook"
  end

  def read_pages
    read_page_offset if @offsets.empty?
    @offsets.map { |pos| Page.new(@f, pos) }
  end

  def read_page_offset
    return [] unless @ok

    @offsets = []

    @f.seek(4)
    @page_count = @f.read(4).get_int4_be
    @offsets = [0]
    @page_count.times { |i|
      offset = @offsets[i] + @f.read(4).get_int4_be
      @offsets.push(offset)
    }

    base = @f.tell
    @offsets.map! { |e| e + base }
  end

end

if __FILE__ == $PROGRAM_NAME
  s = SafariCookie.new "Cookies.binarycookies"
  pages = s.read_pages
  pages.take(50).each { |p| p p.ok }
end
